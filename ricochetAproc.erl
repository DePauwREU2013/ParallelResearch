-module(ricochetAproc).
-compile(export_all).

%%%--------------------------------------------------------------------- 
%%% Description module ricochetAproc
%%%--------------------------------------------------------------------- 
%%% Using Many functions from ricochetRobots module,
%%% this modules solve function returns a solution given a board and 
%%% the color of the active robot.  This search uses a parallel 
%%% version of A*. Given a specific number N the program will assign N 
%%% processes a node to expand. After a process expands it will keep grabbing
%%% the lowest scored node in the shared open queue until a solution is found
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% solve(Board, Color,NumPro,Mode)
%%%   returns a tuple in the form
%%%   {Total Time Search Took, Number of Nodes Expanded, Path Solution}
%%%--------------------------------------------------------------------- 

%%----------------------------------------------------------------------
%% Function: solve/3
%% Purpose: Intialize the search
%% Args:   Board of 32 x 32 2-tuples|Color of active robot|Number of Processes|
%%  	   Either astar or breadthfirst
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solve(Board, Color,NumPro,Mode) ->
    ets:insert(solutions, {time, now()}), 	% Timer starts
    BlankBoard = ricochetRobots:getBlankBoard(Board,[]),		% Used for A* so filled boards are not stored in open ets
    Finish = ricochetRobots:getPosition(Board,finish),		
    ricochetRobots:idealCount(Board,Finish,Mode),				% Calculates "Rook" moves = minimum number of moves to get active(Color) to finish(final)
    AllPositions = [{ricochetRobots:getPosition(Board,red)},{ricochetRobots:getPosition(Board,yellow)},
    				{ricochetRobots:getPosition(Board,green)},{ricochetRobots:getPosition(Board,blue)}],
    cycle(Board,ricochetRobots:getPosition(Board,Color),AllPositions,[],0), % Non recursive call to move each piece in each direction once
    spawnStart(NumPro,[],BlankBoard,Finish,NumPro).

%%----------------------------------------------------------------------
%% Function: spawnStart/5
%% Purpose: Given a number N, create N nodes and after each creation expand in seperate process
%%          If N is too large and a solution is found before N process then main will stop
%%	  	    and wait for everyone to finish
%% Args:   Number of processes left to spawn|List of Process IDs|BlankBoard|
%%         position of finish|number of processes in total
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
spawnStart(0,PidList,BlankBoard,Finish,OrigNumPro)->
    waitForSolution(PidList,BlankBoard,Finish,OrigNumPro);
spawnStart(NumPro,PidList,BlankBoard,Finish,OrigNumPro) ->
    Smallest = ricochetRobots:getSmallest(open,true),
    {{ActivesPosition, AllPositions, Adventured},FScore} = Smallest,
	    if 
	    	ActivesPosition =:= Finish -> % Solution is found early
	    		% Have main process monitor all processes created up to this point and return count
		    	Num = lists:foldl(fun(X,Sum)->erlang:monitor(process,X),Sum+1 end,0,PidList),
		    	ricochetRobots:nodeCount(1),
		    	% Insert solution back into open ets
	    		BinaryAdventured = << <<C:2, D:2>> || {C,D} <- Adventured >>,
	    		ets:insert(open, {{FScore, erlang:phash2(now())},ActivesPosition, AllPositions, BinaryAdventured}),
		    	waitToFinish(Num);
	        true->
				Pid = spawn(ricochetAproc,solveRecursive,[BlankBoard,Smallest,Finish]),
			    spawnStart(NumPro-1,[Pid|PidList],BlankBoard,Finish,OrigNumPro)
		end.

%%----------------------------------------------------------------------
%% Function: solveRecursive/3
%% Purpose: Continually keep sarching until solution is found
%% Args:   BlankBoard|Node with lowest score|Position of finish
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solveRecursive(BlankBoard,Smallest,Finish)->
    {SmallestInfo,FScore} = Smallest,
    {{X,Y}, AllPositions, Adventured} = SmallestInfo,	% {X,Y} is position of active robot		
    [{_,MinMoves}] = ets:lookup(minMoves,16*(X-1)+Y),	% Heuristic 
    Depth = FScore - MinMoves,														
    getBoardAndCycle(BlankBoard,{X,Y},AllPositions,Adventured,Depth), % Expand Smallest Node
    NewSmallest = ricochetRobots:getSmallest(open,true), % true means we want to repeat getSmallest until node is found
	{{NewActivesPosition, NewAllPositions, NewAdventured},NewFScore} = NewSmallest,
	if 
		NewActivesPosition =:= Finish -> % Extracting the Depth
	    	NewNodes = ricochetRobots:nodeCount(1), % put solution node back into open ets
			NewBinaryAdventured = << <<C:2, D:2>> || {C,D} <- NewAdventured >>,
			ets:insert(open, {{NewFScore, NewNodes},NewActivesPosition, NewAllPositions, NewBinaryAdventured}),
	    	main ! found; % wait for all processes to finish and possibly produce better solution
    	true->
    		solveRecursive(BlankBoard,NewSmallest,Finish) % revive process
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
go(Board,ActivesPosition,Position,AllPositions,CurrentColorValue,Direction,Adventured,Depth) ->
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
%% Function: waitForSolution/4
%% Purpose: Main process monitors all processes already made and once
%%          all go down it brings it back up
%% Args: List of process IDs spawn| Number of Processes|BlackBoard|
%%  	 Position of Destination| Color of Active Robot
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
waitForSolution(PidList,BlankBoard,Finish,NumPro) -> 
    lists:foreach(fun(X)->erlang:monitor(process,X) end, PidList), % Monitor all processes
    waitForSolution(BlankBoard,Finish,NumPro).
waitForSolution(BlankBoard,Finish,NumPro) -> 
	% Keep reviving processes until solution is found
    receive
    	found ->  % A solution was found
    		waitToFinish(NumPro);
		{'DOWN',_,_,_PID2,_Reason} ->
		    Smallest = ricochetRobots:getSmallest(open,true),
		    		{{ActivesPosition, AllPositions, Adventured},FScore} = Smallest,
			    	if 
			    		ActivesPosition =:= Finish -> % solution FOUND
					    	Nodes = ricochetRobots:nodeCount(1), % put solution node back into open ets
				    		BinaryAdventured = << <<C:2, D:2>> || {C,D} <- Adventured >>,
				    		ets:insert(open, {{FScore, Nodes},ActivesPosition, AllPositions, BinaryAdventured}),
					    	waitToFinish(NumPro); % wait for all processes to finish and possibly produce better solution
			        	true->
					    	NewPid = spawn(ricochetAproc,solveRecursive,[BlankBoard,Smallest,Finish,NumPro]), % revive process
						    erlang:monitor(process,NewPid),
						    waitForSolution(BlankBoard,Finish,NumPro)
					end
    end.

%%----------------------------------------------------------------------
%% Function: waitToFinish/1
%% Purpose: Solution has been found. Main process monitors returns solution once all processes end
%% Args: Number of Processes left
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
waitToFinish(0) ->	
	{{_ActivesPosition, _AllPositions, Solution},_FScore} = ricochetRobots:getSmallest(open,false), %% grab node with best Fscpre (Guranteed to be optimal solution)
	Output = ricochetRobots:generateOutput(lists:reverse(Solution)),
	ricochetRobots:cleanUp(0),
	Output;
waitToFinish(NumPro) ->
	receive
		found -> 
			waitToFinish(NumPro);
		{'DOWN',_,_,_,_} -> 
			waitToFinish(NumPro-1)
	end.