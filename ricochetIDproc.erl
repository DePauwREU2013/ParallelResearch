-module(ricochetIDproc).
-compile(export_all).

%%%--------------------------------------------------------------------- 
%%% Description module ricochetIDproc
%%%--------------------------------------------------------------------- 
%%% Using Many functions from ricochetRobots module,
%%% this modules solve function returns a solution given a board and 
%%% the color of the active robot.  This search uses a parallel 
%%% version of Iterative Deepening. Given a specific number N, the program
%%% will have N processes solving their own subtrees of the same node tree
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% solve(Board, Color,NumPro)
%%%   returns a tuple in the form
%%%   {Total Time Search Took, Number of Nodes Expanded, Path Solution}
%%%--------------------------------------------------------------------- 

%%----------------------------------------------------------------------
%% Function: solve/3
%% Purpose: Intialize the search
%% Args:   Board of 32 x 32 2-tuples|Color of active robot|Max Depths to Spawn Processes
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solve(Board, Color, NumPro) ->
    ets:insert(solutions, {time, now()}),		% Timer starts
    ets:insert(numProc, {numPro, NumPro}),
    Finish = ricochetRobots:getPosition(Board,finish),
    ricochetRobots:idealCount(Board,Finish,id),
    AllPositions = [{ricochetRobots:getPosition(Board,red)},{ricochetRobots:getPosition(Board,yellow)},
    				{ricochetRobots:getPosition(Board,green)},{ricochetRobots:getPosition(Board,blue)}],
    solveRecursive(Board,Finish,Color,AllPositions,1).

%%----------------------------------------------------------------------
%% Function: solveRecursive/3
%% Purpose: Continually keep sarching until solution is found
%% Args:   Board|Destination for Active Robot|Color of active robot|
%%  	   Ordered Positions of all robots|Turn Count|Max Depths to Spawn Processes
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solveRecursive(Board,Finish,Color,AllPositions,Acc)->
    ActivesPosition = ricochetRobots:getPosition(Board,Color),
    Pid = spawn(ricochetIDproc,cycle,[Board,ActivesPosition,AllPositions,Finish,[],Acc]), % Expand Current Node
    main ! {add,Pid}, % Tells main to monitor this process
    waitToFinish(0,first), % Tells main to sit and wait for messages
    Solutions = ets:lookup(solutions,found),
    case Solutions of
		[] -> 
			solveRecursive(Board,Finish,Color,AllPositions,Acc+1); % No solution, Keep searching  
		[{found,Solution}] ->
			Output = ricochetRobots:generateOutput(Solution),
			ricochetRobots:cleanUp(0),
			Output;
		_ -> 
			invalidSolution
    end.

%%----------------------------------------------------------------------
%% Function: cycle/6
%% Purpose: Checks if active Robot is at finish position. Make certain this board 
%%  		configuration doesnt already exists.
%%          Add it to configurations checked and then expand by Moving each piece.
%%          Note that the swapping 2 non active robots yeilds the same configuration
%% Args: Board|Active Robots Position|Ordered Position of Robots|Position of Finish
%%       Path so far| number of Turns Left  
%% Returns: [] if no soution
%%          true if solution inserted into solutions ets
%%          other atom signifying reason for stopping the search
%%----------------------------------------------------------------------
cycle(Board,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft)->
	ricochetRobots:nodeCount(-1),
    if 
    	ActivesPosition =:= Finish -> % robot is at final destination
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
				true ->	% this configuration has been done at a certain depth
					[{_,TurnCount}] = MatchingList
			end,
			if 
				TurnCount >= TurnsLeft -> % This configuration has been done in the same or fewer moves 
					beenDone;
				MinMoves > TurnsLeft -> % Given how far away active robot is its not possible with number of TurnsLeft
					cannotBeDone;
		       	true -> 
		       		ets:insert(configs, {HashValue, TurnsLeft}),
			       	move(Board,Red,0,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft),
			       	move(Board,Yellow,1,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft),
			       	move(Board,Green,2,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft),
			       	move(Board,Blue,3,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,TurnsLeft)
		    end
    end.

%%----------------------------------------------------------------------
%% Function: move/8
%% Purpose: Given a certain piece try to move it in any possible direction.
%%          Possible direction means there isnt a wall and if the most recent move 
%%          was this piece then it cant move in the oppsite direction it traveled
%% Args: Board|Position of Piece being moved|Color Value between 0 - 3|Active Robots Position|
%%       Ordered Position of Robots| Position of Finish| Path so far| Number o Turns Left
%% Returns: [] if no soution
%%          true if solution inserted into solutions ets
%%			other atom signifying reason for stopping the search
%%----------------------------------------------------------------------
move(Board,Position,ColorBit,ActivesPosition,AllPositions,Finish,Adventured,TurnsLeft)->    
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
	    	go(Board,ActivesPosition,ColorBit, Position,AllPositions,CurrentColorValue,0,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious);
       	true -> northWall
    end,
    if 
    	CurrentValue rem 5 =/= 0, (PreviousDirection =/= 3 orelse ColorBit =/= PreviousRobot)  -> % Robot Moving east							
	    	go(Board,ActivesPosition,ColorBit, Position,AllPositions,CurrentColorValue,1,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious);
       	true -> eastWall
    end,
    if 
    	CurrentValue rem 7 =/= 0, (PreviousDirection =/= 0 orelse ColorBit =/= PreviousRobot)  -> % Robot Moving south							
	    	go(Board,ActivesPosition,ColorBit, Position,AllPositions,CurrentColorValue,2,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious);
       	true -> southWall
    end,
    if 
    	CurrentValue rem 11 =/= 0, (PreviousDirection =/= 1 orelse ColorBit =/= PreviousRobot)  ->	% Robot Moving west							
	    	go(Board,ActivesPosition,ColorBit, Position,AllPositions,CurrentColorValue,3,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious);
       	true -> westWall
    end.

%%----------------------------------------------------------------------
%% Function: go/12
%% Purpose: Given a piece and direction it moves the piece as far as possible
%%          in that direction. The new node created with various values is stored in the
%%          open priority queue
%% Args: Board|Active Robots Position|Color Value between 0 - 3|Position of Piece being moved|
%%       Ordered Position of Robots| Prime Assigned Color Value| Direction Moving | 
%%       Position of Finish| Path so far| Turns Left| Board posisiton being changed/deleted|
%%  	 Board position being updated
%% Returns: [] if no soution
%%          true if solution inserted into solutions ets
%%			other atom signifying reason for stopping the search
%%----------------------------------------------------------------------
go(Board,ActivesPosition, ColorBit,Position,AllPositions,CurrentColorValue,Direction,Finish,Adventured,TurnsLeft,OldPrevious,NewPrevious) ->
    % Position which the current piece is moving to
    {Row,Col} = ricochetRobots:checkPath(Board,Position,Direction), % Spot robot is moving TO
   	OldValue = ricochetRobots:getValue(Board,{Row,Col}), % Value at above position BEFORE being occupied
    if 
        % Robot trying to move to same location means an occupied position is 1 spot away
    	Position =:= {Row,Col} -> 
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
		    % Tell main process node was expanded, wait for instructions
		    main ! {procFin,self(),[NewBoard,ActiveNewPosition,NewPositions,Finish,[{ColorBit,Direction}|Adventured],TurnsLeft - 1]},
		    ricochetRobots:nodeCount(1),
		    receive 
				didSpawn-> % main process spawned new process by calling cycle on its own
			    	done;
				noSpawn-> % main process decided there are enough processes, no need for more
			    	cycle(NewBoard,ActiveNewPosition,NewPositions,Finish,[{ColorBit,Direction}|Adventured],TurnsLeft - 1)
		    end
    end.

%%----------------------------------------------------------------------
%% Function: waitToFinish/2
%% Purpose: Waits for previous spawn processes to all finish expanding their tree.
%% 			Tries to make certain there are always a specific number of processes
%% Args: Number of Processes left|atom signfying whether this was the first time this function was called
%% Returns: done 
%%----------------------------------------------------------------------
waitToFinish(0,notFirst) ->
    done;
waitToFinish(PidCount, _First) ->
    receive
		{procFin, From, Params} ->
			% N is the number or processes we need to spawn
		    [{numPro,N}] = ets:lookup(numProc,numPro),
		    if 
		    	N > 0 ->
				    From ! didSpawn, % tell the process that we handled its call to cycle
				    NewPid = spawn(ricochetIDproc,cycle,Params),
				    erlang:monitor(process,NewPid),
				    ets:insert(numProc,{numPro, N-1}),
				    waitToFinish(PidCount+1,notFirst);
		    	true ->
		    		% Dont need more processes
					From ! noSpawn,
					waitToFinish(PidCount,notFirst)
			end;
		{add, Pid} -> 
			erlang:monitor(process,Pid),
			[{numPro,N}] = ets:lookup(numProc,numPro),
			ets:insert(numProc,{numPro, N-1}),
			waitToFinish(PidCount+1,notFirst);
		{'DOWN',_,_,_,_} ->
			[{numPro,N}] = ets:lookup(numProc,numPro),
			ets:insert(numProc,{numPro, N+1}),
			waitToFinish(PidCount-1,notFirst)
    end.