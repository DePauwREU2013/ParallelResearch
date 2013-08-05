-module(ricochetA).
-compile(export_all).

%%%--------------------------------------------------------------------- 
%%% Description module ricochetA
%%%--------------------------------------------------------------------- 
%%% Using Many functions from ricochetRobots module,
%%% this modules solve function returns a solution given a board and 
%%% the color of the active robot.  This search uses A*. This means
%%% that intial nodes formed from the first move are put into a
%%% priority queue with a score. The score is the depth the node is 
%%% at plus the heuristic. The lowest score is expanded and if a 
%%% the same process
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
%% Purpose: Continually keep sarching until solution is found
%% Args:   BlankBoard|Destination for Active Robot|Color of active robot
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solveRecursive(BlankBoard,Finish,Color)->
    % Get 1 node with the smallest score
    {{{X,Y}, AllPositions, Adventured},FScore} = ricochetRobots:getSmallest(open,true), 
    if 
    	{X,Y} =:= Finish -> % active robot is at finish
			Output = ricochetRobots:generateOutput(lists:reverse(Adventured)),
			ricochetRobots:cleanUp(0),
			Output;
    	true->  
	    	[{_,MinMoves}] = ets:lookup(minMoves,16*(X-1)+Y),		% Heuristic
		    Depth = FScore - MinMoves,								% Extracting the Depth
		    getBoardAndCycle(BlankBoard,{X,Y},AllPositions,Adventured,Depth), 	% No solution, spawn next level  
		    solveRecursive(BlankBoard,Finish,Color) % Check for solution
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
cycle(Board,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth)->
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
		    move(Board,Red,0,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth),
		    move(Board,Yellow,1,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth),
		    move(Board,Green,2,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth),
		    move(Board,Blue,3,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth)
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
    	CurrentValue rem 11 =/= 0, (PreviousDirection =/= 1 orelse CurrentColorValue =/= PreviousRobot) -> % Robot Moving west
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
		    [{_,MinMoves}] = ets:lookup(minMoves,16*(ActiveRow-1)+ActiveCol), % Get Heuristic
		    ricochetRobots:nodeCount(1), 
		    FScore = (Depth + 1) + MinMoves, % Calculate score
            % The Path so far is converted here to save memory
		    BinaryAdventured = << <<C:2, D:2>> || {C,D} <- [{CurrentColorValue, Direction}|Adventured] >>,
            % The key for any node is its score and a unique number to prevent overwrites
		    ets:insert(open, {{FScore, erlang:phash2(now())},ActiveNewPosition, NewPositions, BinaryAdventured})		
    end.