-module(ricochetIDlevel).
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
%% Args:   Board of 32 x 32 2-tuples|Color of active robot|Max Depths to Spawn Processes
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solve(Board, Color,NumPro) ->
	ets:insert(solutions, {time, now()}),		% Timer starts
	Finish = ricochetRobots:getPosition(Board,finish),
	ricochetRobots:idealCount(Board,Finish,id),
	AllPositions = [{ricochetRobots:getPosition(Board,red)},{ricochetRobots:getPosition(Board,yellow)},
					{ricochetRobots:getPosition(Board,green)},{ricochetRobots:getPosition(Board,blue)}],
	solveRecursive(Board,Finish,Color,AllPositions,1,NumPro).

%%----------------------------------------------------------------------
%% Function: solveRecursive/3
%% Purpose: Continually keep sarching until solution is found
%% Args:   Board|Destination for Active Robot|Color of active robot|
%%  	   Ordered Positions of all robots|Turn Count|Max Depths to Spawn Processes
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solveRecursive(Board,Finish,Color,AllPositions,Turn,NumPro)->
    ActivesPosition = ricochetRobots:getPosition(Board,Color),
    cycle(Board,ActivesPosition,AllPositions,Finish,[],Turn,NumPro), % Expand Current Node		
    Solutions = ets:lookup(solutions,found), % See if expansion got us an answer
    case Solutions of
		[] -> % No solution, Keep searching
			solveRecursive(Board,Finish,Color,AllPositions,Turn + 1,NumPro); 
		[{found,Solution}] ->
			Output = ricochetRobots:generateOutput(Solution),
			ricochetRobots:cleanUp(0),
			Output;
		_ -> 
			invalidSolution
    end.

%%----------------------------------------------------------------------
%% Function: cycle/7
%% Purpose: Checks if Active Robot is at finish position. Make certain this board 
%%  		configuration doesnt already exists.
%%          Add it to configurations checked and then expand by Moving each piece.
%%          Note that the swapping 2 non active robots yeilds the same configuration
%% Args: Board|Active Robots Position|Ordered Position of Robots| Position of Finish
%%       Path so far| Turns Left| Max Depth to spawn processes at
%% Returns: [] if no soution
%%          true if solution inserted into solutions ets
%%			other atom signifying reason for stopping the search
%%----------------------------------------------------------------------
cycle(Board,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft,NumProcess)->
	ricochetRobots:nodeCount(-1),
	if 
		ActivesPosition =:= Finish -> % Robot is at final destination
			ets:insert(solutions, {found, lists:reverse(Adventured)});
    	TurnsLeft =:= 0 -> % Cannot make anymore moves
			[];
    	true->
			ThreeNonActives = [Red,Yellow,Green,Blue]--[ActivesPosition],
			[NonActive1,NonActive2,NonActive3] = lists:sort(ThreeNonActives),
			% Only reason to hash this configuration is to save memory
			HashValue = erlang:phash2({ActivesPosition,NonActive1,NonActive2,NonActive3}),
			{X,Y} = ActivesPosition,
			MatchingList = ets:lookup(configs,HashValue),
			[{_,MinMoves}] = ets:lookup(minMoves,16*(X-1)+Y),
			if 
				MatchingList =:= [] -> % this configuration has never been done
					TurnCount = 0;
				true -> % this configuration has been done at a certain depth
					[{_,TurnCount}] = MatchingList
			end,
			if 
				TurnCount >= TurnsLeft ->  % this configuration has been done in the same or fewer moves 
					beenDone;
				MinMoves > TurnsLeft -> % Given how far away active robot is its not possible with number of TurnsLeft
					cannotBeDone;
				true -> 
					ets:insert(configs, {HashValue, TurnsLeft}),
					if 
						NumProcess =:= 0 -> % Continue expanding in same process
							move(Board,Red,0,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft,NumProcess),
							move(Board,Yellow,1,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft,NumProcess),
							move(Board,Green,2,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft,NumProcess),
							move(Board,Blue,3,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft,NumProcess);
						true ->	% Spawn a process to move each piece and then wait for all to finish the tree they create
					       	Pid1 = spawn(ricochetIDlevel, move,[Board,Red,0,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft,NumProcess-1]),
					       	Pid2 = spawn(ricochetIDlevel, move,[Board,Yellow,1,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft,NumProcess-1]),
							Pid3 = spawn(ricochetIDlevel, move,[Board,Green,2,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft,NumProcess-1]),
							Pid4 = spawn(ricochetIDlevel, move,[Board,Blue,3,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft,NumProcess-1]),
							waitToFinish([Pid1,Pid2,Pid3,Pid4],4)				
					end
			end
	end.

%%----------------------------------------------------------------------
%% Function: move/9
%% Purpose: Given a certain piece try to move it in any possible direction.
%%          Possible direction means there isnt a wall and if the most recent move 
%%          was this piece then it cant move in the oppsite direction it traveled
%% Args: Board|Position of Piece being moved|Color Value between 0 - 3|Active Robots Position|
%%       Ordered Position of Robots| Position of Finish| Path so far| Depth currently before expansion|
%%  	 Max Depth to spawn processes at
%% Returns: [] if no soution
%%          true if solution inserted into solutions ets
%%			other atom signifying reason for stopping the search
%%----------------------------------------------------------------------
move(Board,Position,ColorBit,ActivesPosition,AllPositions,Finish,Adventured,TurnsLeft,NumProcess)->    
	case Adventured of
		[] -> % very first turn
			PreviousRobot = undefined,
			PreviousDirection = undefined;
    	_ ->
    		[{PreviousRobot,PreviousDirection} | _Rest] = Adventured
	end,
	% This value tells you if an obstacle exists in any direction
	CurrentValue = ricochetRobots:getValue(Board,Position), 
	[CurrentColorValue] = [Value || Value<-[13,17,19,23],CurrentValue rem Value =:= 0],       		
	OldPrevious = {Position,CurrentValue}, % Spot robot is moving FROM 
	% Spot moving FROM needs to lose property of occupied and Color Value from Robot
	NewPrevious = {Position,CurrentValue div (CurrentColorValue*2)}, 
	if 
		CurrentValue rem 3 =/= 0, (PreviousDirection =/= 2 orelse ColorBit =/= PreviousRobot) -> % Robot Moving north						
	   		go(Board,ActivesPosition,ColorBit,Position,AllPositions,CurrentColorValue,0,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious,NumProcess);
	   	true -> northWall
	end,
	if 
		CurrentValue rem 5 =/= 0, (PreviousDirection =/= 3 orelse ColorBit =/= PreviousRobot)  -> % Robot Moving east							
	   		go(Board,ActivesPosition,ColorBit,Position,AllPositions,CurrentColorValue,1,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious,NumProcess);
	   	true -> eastWall
	end,
	if 
		CurrentValue rem 7 =/= 0, (PreviousDirection =/= 0 orelse ColorBit =/= PreviousRobot)  -> % Robot Moving south							
	   		go(Board,ActivesPosition,ColorBit,Position,AllPositions,CurrentColorValue,2,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious,NumProcess);
	  	true -> southWall
	end,
	if 
		CurrentValue rem 11 =/= 0, (PreviousDirection =/= 1 orelse ColorBit =/= PreviousRobot)  ->	% Robot Moving west							
	   		go(Board,ActivesPosition,ColorBit,Position,AllPositions,CurrentColorValue,3,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious,NumProcess);
	   	true -> westWall
	end.

%%----------------------------------------------------------------------
%% Function: go/13
%% Purpose: Given a piece and direction it moves the piece as far as possible
%%          in that direction. The new node created with various values is stored in the
%%          open priority queue
%% Args: Board|Active Robots Position|Color Value between 0 - 3|Position of Piece being moved|
%%       Ordered Position of Robots| Prime Assigned Color Value| Direction Moving | 
%%       Position of Finish| Path so far| Turns Left| Board posisiton being changed/deleted|
%%  	 Board position being updated| Max Depth to spawn processes at
%% Returns: [] if no soution
%%          true if solution inserted into solutions ets
%%			other atom signifying reason for stopping the search
%%----------------------------------------------------------------------
go(Board,ActivesPosition, ColorBit, Position,AllPositions,CurrentColorValue,Direction,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious,NumProcess) ->
   	{Row,Col} = ricochetRobots:checkPath(Board,Position,Direction), % Position robot is moving TO
   	OldValue = ricochetRobots:getValue(Board,{Row,Col}), % Value at above position BEFORE being occupied
	if 
   		Position =:= {Row,Col} -> % Robot trying to move to current location means an occupied position is 1 spot away
   			blocked;
   		true-> 
			NewPositions = lists:keyreplace(Position,1,AllPositions,{{Row,Col}}),
		  	% Spot moving TO needs updated value to signify occupancy and color of robot at it
			NewCurrent = {{Row,Col},OldValue * CurrentColorValue * 2},  
			% New Board with old MOVING TO and MOVING FROM spots removed, and updated spots added
			NewBoard = lists:sort([NewPrevious,NewCurrent|Board--[OldPrevious,{{Row,Col},OldValue}]]),
		    if 
		    	Position =:= ActivesPosition ->
			    	ActiveNewPosition = {Row,Col}; % if we just moved active robot we need to update its position
		       	true ->
			    	ActiveNewPosition = ActivesPosition
		    end,
		    ricochetRobots:nodeCount(1),
		    cycle(NewBoard,ActiveNewPosition,NewPositions,Finish,[{ColorBit,Direction}|Adventured],TurnsLeft - 1,NumProcess)		    
	end.

%%----------------------------------------------------------------------
%% Function: waitToFinish/2
%% Purpose: Waits for previous spawn processes to all finish expanding their tree
%% Args: List of Process IDs|Number of Processes
%% Returns: done 
%%----------------------------------------------------------------------
waitToFinish(PidList,PidCount) -> 
    lists:foreach(fun(X)->erlang:monitor(process,X) end, PidList),
    waitToFinish(PidCount).
waitToFinish(0) ->
    done;
waitToFinish(PidCount) ->
    receive
		{'DOWN',_,_,_,_} ->
	    	waitToFinish(PidCount-1)
    end.