-module(ricochetAlevel).
-compile(export_all).

%%%--------------------------------------------------------------------- 
%%% Description module ricochetAlevel
%%%--------------------------------------------------------------------- 
%%% Using Many functions from ricochetRobots module,
%%% this modules solve function returns a solution given a board and 
%%% the color of the active robot.  This search uses a parallel 
%%% version of A*. After the initial nodes are created the search will
%%% find all nodes of the lowest score and expand them all at the 
%%% same time. If a search is found then the answer is returned, 
%%% otherwise the process repeats.
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% solve(Board, Color,Mode)
%%%   returns a tuple in the form
%%%   {Total Time Search Took, Number of Nodes Expanded, Path Solution}
%%%--------------------------------------------------------------------- 

%%----------------------------------------------------------------------
%% Function: solve/3
%% Purpose: Intialize the search
%% Args:   Board of 32 x 32 2-tuples|Color of active robot|Either astar or breadthfirst
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solve(Board, Color,Mode) ->
    ets:insert(solutions, {time, now()}), 	% Timer starts
    BlankBoard = ricochetRobots:getBlankBoard(Board,[]),		% Board with no pieces
    Finish = ricochetRobots:getPosition(Board,finish),		
    ricochetRobots:idealCount(Board,Finish,Mode),				% Calculates heuristics
    AllPositions = [{ricochetRobots:getPosition(Board,red)},{ricochetRobots:getPosition(Board,yellow)},
    				{ricochetRobots:getPosition(Board,green)},{ricochetRobots:getPosition(Board,blue)}],
    % Non recursive call to move each piece in each possible direction once
    cycle(Board,ricochetRobots:getPosition(Board,Color),AllPositions,[],0), 
    solveRecursive(BlankBoard,Finish,Color).

%%----------------------------------------------------------------------
%% Function: solveRecursive/3
%% Purpose: Get a list of all nodes with the smallest score
%% Args:   BlankBoard|Destination for Active Robot|Color of active robot
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solveRecursive(BlankBoard,Finish,Color)->
    {NextSmallestList,FScore} = ricochetRobots:getSmallest(open,all,[]), 							
    spawnRecursive(BlankBoard,Finish,Color,NextSmallestList,FScore,[],0).

%%----------------------------------------------------------------------
%% Function: spawnRecursive/7
%% Purpose: Spawn a process for expanding each node in a list. If a solution is found
%%  		before all processes are spawned then the main process jumps directly to a
%%  		function which waits for all process spawned up to the point to stop. Also 
%%  		when a solution is found it is put back into the open ets for easy retrieval 
%%  		and if a better solution is found then it will ensure the better one is retrieved instead
%% Args:   BlankBoard|Destination for Active Robot|Color of active robot|
%%         List of nodes to spawn|score of lowest node|number of processes left to spawn
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
spawnRecursive(BlankBoard,Finish,Color,[],_,ListOfPids,NumPro) -> 
	% everyone is spawn, main process will wait until a solution is found
	waitForSolution(ListOfPids,NumPro,BlankBoard,Finish,Color);			
spawnRecursive(BlankBoard,Finish,Color,[FirstSmallest|Rest],FScore,ListOfPids,NumPro) ->	
    {{X,Y}, AllPositions, Adventured} = FirstSmallest,
    [{_,MinMoves}] = ets:lookup(minMoves,16*(X-1)+Y),	
    if 
    	{X,Y} =:= Finish -> % active robot is at final destination
			ricochetRobots:nodeCount(1), 
			ets:insert(solutions, {found, lists:reverse(Adventured)}),
		    waitToFinish(ListOfPids,NumPro);
    	true -> 
			Depth = FScore - MinMoves,
		    Pid = spawn(ricochetAlevel,getBoardAndCycle,[BlankBoard,{X,Y},AllPositions,Adventured,Depth]),
		    spawnRecursive(BlankBoard,Finish,Color,Rest,FScore,[Pid|ListOfPids],NumPro+1)
	end. 

%%----------------------------------------------------------------------
%% Function: getBoardAndCycle/5
%% Purpose: Put the pieces on the board and begin the expansion
%% Args: BlankBoard|Active Robots Position|Ordered Position of Robots|
%%       Path so far| Depth currently before expansion   
%% Returns: true 
%%----------------------------------------------------------------------
getBoardAndCycle(BlankBoard, ActivePosition, AllPositions, Adventured, Depth)->
    FilledBoard = ricochetRobots:getFilledBoard(BlankBoard, AllPositions, []),
    cycle(FilledBoard,ActivePosition, AllPositions,Adventured,Depth).

%%----------------------------------------------------------------------
%% Function: cycle/5
%% Purpose: Make certain this board configuration doesnt already exists
%%          Add it to configurations checked and then expand by Moving each piece.
%%          Note that the swapping 2 non active robots yeilds the same configuration
%% Args: Board|Active Robots Position|Ordered Position of Robots|
%%       Path so far| Depth currently before expansion   
%% Returns: true 
%%----------------------------------------------------------------------
cycle(NewBoard,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth)->
	ThreeNonActives = [Red,Yellow,Green,Blue]--[ActivesPosition],
	[NonActive1,NonActive2,NonActive3] = lists:sort(ThreeNonActives),
    % Only reason to hash this configuration is to save memory
	HashValue = erlang:phash2({ActivesPosition,NonActive1,NonActive2,NonActive3}),
	MatchingList = ets:lookup(configs,HashValue),
    if 
    	MatchingList =:= [] -> % this configuration has never been done
    		DepthDone = infinity;
       	true -> % this configuration has been done at a certain depth
       		[{_,DepthDone}] = MatchingList
    end,
    if 
    	DepthDone =< Depth ->  % this configuration has been done in the same or fewer moves
    		beenDone;
	  	true ->
		    ets:insert(configs, {HashValue, Depth}),
		    move(NewBoard,Red,0,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth),
		    move(NewBoard,Yellow,1,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth),
		    move(NewBoard,Green,2,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth),
		    move(NewBoard,Blue,3,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth)
	end.

%%----------------------------------------------------------------------
%% Function: move/7
%% Purpose: Given a certain piece try to move it in any possible direction.
%%          Possible direction means there isnt a wall and if the most recent move 
%%          was this piece then it cant move in the oppsite direction it traveled
%% Args: Board|Position of Piece being moved|Color Value between 0 - 3|Active Robots Position|
%%       Ordered Position of Robots| Path so far| Depth currently before expansion   
%% Returns: true 
%%----------------------------------------------------------------------
move(Board,Position,CurrentColorValue,ActivesPosition,AllPositions,Adventured,Depth)->    
	case Adventured of
		[] -> % very first turn
			PreviousRobot = undefined,
			PreviousDirection = undefined;
    	_ ->
    		[{PreviousRobot,PreviousDirection} | _Rest] = Adventured
	end,
    % This value tells you if an obstacle exists in any direction
    CurrentValue = ricochetRobots:getValue(Board,Position), 
    if 
    	CurrentValue rem 3 =/= 0, (PreviousDirection =/= 2 orelse CurrentColorValue =/= PreviousRobot) -> % Robot Moving north	
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,0,Adventured,Depth);
       	true -> northWall
    end,
    if 
    	CurrentValue rem 5 =/= 0, (PreviousDirection =/= 3 orelse CurrentColorValue =/= PreviousRobot)  -> % Robot Moving east
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,1,Adventured,Depth);
       	true -> eastWall
    end,
    if 
    	CurrentValue rem 7 =/= 0, (PreviousDirection =/= 0 orelse CurrentColorValue =/= PreviousRobot)  -> % Robot Moving south
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,2,Adventured,Depth);
       	true -> southWall
    end,
    if 
    	CurrentValue rem 11 =/= 0, (PreviousDirection =/= 1 orelse CurrentColorValue =/= PreviousRobot)  ->	% Robot Moving west
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,3,Adventured,Depth);
       	true -> westWall
    end.

%%----------------------------------------------------------------------
%% Function: go/8
%% Purpose: Given a piece and direction it moves the piece as far as possible
%%          in that direction. The new node created with various values is stored in the
%%          open priority queue
%% Args: Board|Active Robots Position|Position of Piece being moved| Ordered Position of Robots|
%%       Color Value between 0-3|Desired Direction Path so far| Depth currently before expansion   
%% Returns: true 
%%----------------------------------------------------------------------
go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,Direction,Adventured,Depth) ->
    % Position which the current piece is moving to
	{Row,Col} = ricochetRobots:checkPath(Board,Position,Direction),
    if 
        % Robot trying to move to same location means an occupied position is 1 spot away
    	Position =:= {Row,Col} -> 
			blocked;
    	true-> 
			NewPositions = lists:keyreplace(Position,1,AllPositions,{{Row,Col}}),
		    if 
		    	Position =:= ActivesPosition ->
			    	ActiveNewPosition = {Row,Col}; % if we just moved active robot we need to update its position
		       	true ->
			    	ActiveNewPosition = ActivesPosition
		    end,
		    {ActiveRow, ActiveCol} = ActiveNewPosition,
		    [{_,MinMoves}] = ets:lookup(minMoves,16*(ActiveRow-1)+ActiveCol),
		    FScore = (Depth + 1) + MinMoves, % Calculate score
		    ricochetRobots:nodeCount(1),
		    % The Path so far is converted here to save memory
		    BinaryAdventured = << <<C:2, D:2>> || {C,D} <- [{CurrentColorValue, Direction}|Adventured] >>,
            % The key for any node is its score and a unique number to prevent overwrites
		    ets:insert(open, {{FScore, erlang:phash2(now())},ActiveNewPosition, NewPositions, BinaryAdventured})		
    end.

%%----------------------------------------------------------------------
%% Function: waitForSolution/5
%% Purpose: Main process monitors all processes already made and once
%%          all go down it calls solveRecursive at the top to repeat 
%% Args: List of process IDs spawn| Number of Processes|BlackBoard|
%%  	 Position of Destination| Color of Active Robot
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
waitForSolution(PidList,NumPro,BlankBoard,Finish,Color) -> 
    lists:foreach(fun(X)->erlang:monitor(process,X) end, PidList),
    waitForSolution(NumPro,BlankBoard,Finish,Color).
waitForSolution(0,BlankBoard,Finish,Color) ->
    solveRecursive(BlankBoard,Finish,Color);
waitForSolution(NumPro,BlankBoard,Finish,Color) ->
    receive
		{'DOWN',_,_,_,_} ->
	    	waitForSolution(NumPro-1,BlankBoard,Finish,Color)
    end.

%%----------------------------------------------------------------------
%% Function: waitToFinish/2
%% Purpose: Solution has been found. Main process monitors all processes already 
%%          made and once all go down it terminates the program, returning solution 
%% Args: List of process IDs spawn| Number of Processes
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
waitToFinish(PidList,NumPro) -> 
    lists:foreach(fun(X)->erlang:monitor(process,X) end, PidList),
    waitToFinish(NumPro).
waitToFinish(0)->	
	% Obtain node with best Fscore (Guranteed to be optimal solution)
	[{found,Solution}] = ets:lookup(solutions,found),
	Output = ricochetRobots:generateOutput(Solution),
	ricochetRobots:cleanUp(0),
	Output;
waitToFinish(NumPro) ->
	receive
		{'DOWN',_,_,_,_} -> waitToFinish(NumPro-1)
	end.