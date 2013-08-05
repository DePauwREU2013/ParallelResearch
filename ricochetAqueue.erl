-module(ricochetAqueue).
-compile([debug_info, export_all]).

%%%--------------------------------------------------------------------- 
%%% Description module ricochetAqueue
%%%--------------------------------------------------------------------- 
%%% Using Many functions from ricochetRobots module,
%%% this modules solve function returns a solution given a board and 
%%% the color of the active robot.  This search uses a parallel 
%%% version of A*. Given a specific number N the program will assign N 
%%% processes an ets with at least 1 node in it. Each process has an independent
%%% open priority queue but all share a closed (already been done) list.
%%% Each process continually expands the lowest scoring node until a 
%%% a solution is found, Once a solution is found, all the other process
%%% must "catch-up" to that solution to be certain a better solution
%%% does not get skipped
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% solve(Board, Color,NumPro,Mode)
%%%   returns a tuple in the form
%%%   {Total Time Search Took, Number of Nodes Expanded, Path Solution}
%%%--------------------------------------------------------------------- 

%%----------------------------------------------------------------------
%% Function: solve/4
%% Purpose: Intialize the search
%% Args:   Board of 32 x 32 2-tuples|Color of active robot|Number of processes| Either astar or breadthfirst
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solve(Board, Color, NumPro, Mode) ->
    ets:insert(solutions, {time, now()}), 	% Timer starts
    BlankBoard = ricochetRobots:getBlankBoard(Board,[]),		% Used for A* so filled boards are not stored in open ets
    Finish = ricochetRobots:getPosition(Board,finish),		
    ricochetRobots:idealCount(Board,Finish,Mode),				% Calculates "Rook" moves = minimum number of moves to get active(Color) to finish(final)
    AllPositions = [{ricochetRobots:getPosition(Board,red)},{ricochetRobots:getPosition(Board,yellow)},
    				{ricochetRobots:getPosition(Board,green)},{ricochetRobots:getPosition(Board,blue)}],
    cycle(Board,ricochetRobots:getPosition(Board,Color),AllPositions,[],0,open), % Non recursive call to move each piece in each direction once
    solveRecursive(BlankBoard,Finish,Color,NumPro,open). % !! MUST BE UNDER 2034 !!

%%----------------------------------------------------------------------
%% Function: solveRecursive/5
%% Purpose: Continually keep sarching until there are NumPro number of nodes
%%  		Once this is achieved then split the nodes up into NumPro number of
%%  		seperate ETSs, one for each process. These process cycle until a solution is found,
%%  		or until they have caught up to a found solution
%% Args:   BlankBoard|Position of finish|Color of active robot|Number of processes|Assigned ETS
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solveRecursive(BlankBoard,Finish,Color,NumPro,ETS)->
    NumNodes = ricochetRobots:nodeCount(get),
    if 
    	% next line only true the first time we have at least NumPro number of nodes
    	NumPro > 0 andalso NumNodes >= NumPro -> 
    		% Gets a list of all nodes from open ets to put into FillETS
		    fillETS(NumPro,0,ets:tab2list(ETS)), 
		    % After all ETSs are filled, start expanding
		    PidList = createProcesses(NumPro,BlankBoard,Finish,Color,[]),
		    % Main process will monitor all those created and wait until a solution is found and finish
		    waitToFinish(add,PidList,NumPro);
    	true->     
    		% false parameter is nessesary because ETS might be empty
		    Smallest = ricochetRobots:getSmallest(ETS,false),
		    if 
		    	Smallest =:= empty -> 
		    		% This process cannot expand anymore nodes and dies
		    		ETS;
		    	true -> 
				    {{{X,Y}, AllPositions, Adventured},FScore} = Smallest,	% {X,Y} is position of active robot
				    [{_,MinMoves}] = ets:lookup(minMoves,16*(X-1)+Y),		% Heuristic used in A*
				    Depth = FScore - MinMoves,
				    Solutions = ets:lookup(solutions,found),
				    case Solutions of
						[] -> % no solution already exists												
						    if
						    	{X,Y} =:= Finish -> % Current Node has solution
						    		ets:insert(solutions, {found, {Depth,lists:reverse(Adventured)}});
						    	true -> % no solution, try again
								    getBoardAndCycle(BlankBoard,{X,Y},AllPositions,Adventured,Depth,ETS),
								    solveRecursive(BlankBoard,Finish,Color,NumPro,ETS)
							end;
						_ -> % SOME process found a solution but this process neds to "catch up" since its still
							 % possible it can find a better solution
						    [{found,{SolutionDepth,_}}] = Solutions,
						    if
						    	{X,Y} =:= Finish -> % Current Node has solution
						    		if 
				    					SolutionDepth > Depth -> % Current Node has a solution better
					    					ets:insert(solutions, {found, {Depth,lists:reverse(Adventured)}});
				       					true ->
					    					done % This process/ets's job is done
				    				end;
						    	true ->
								    if 
								    	FScore < SolutionDepth -> % Possible for a better solution to be found
										    getBoardAndCycle(BlankBoard,{X,Y},AllPositions,Adventured,Depth,ETS),
										    solveRecursive(BlankBoard,Finish,Color,NumPro,ETS);
								       	true ->  % SOME process found a solution AND this process cant do possibly find a better one
										     if 
										     	NumPro > 0 -> % a solution was found BEFORE we can get numpro number of nodes
										     		waitToFinish(0,0); % Will properly end the the program
										     	true -> 
										     		done % This process/ets's job is done
										     end
								    end
							end
				    end
		    end
    end.

%%----------------------------------------------------------------------
%% Function: fillETS/3
%% Purpose: Create a specific number N of priority queues (ETS). They are
%%          named as an integer turned into a list turned into an atom.
%%          Incase we have more nodes than ETSs bing created then we use
%%			rem to be sure all nodes are accounted for. 
%% Args:   Number of processes total|Number of nodes accounted for so far|
%%         List of nodes to put into ETSs
%% Returns: filled atom
%%----------------------------------------------------------------------
fillETS(_,_,[])->
    filled;
fillETS(NumPro,Acc,[MainHead|Tail])->
    List = integer_to_list(Acc rem NumPro),
    AtomizedInt = list_to_atom(List),
    if 
    	Acc >= NumPro ->
			etsExists;
       	true ->
	    	ets:new(AtomizedInt,[ordered_set,named_table,public])
    end, 
    ets:insert(AtomizedInt,MainHead),
    fillETS(NumPro,Acc+1,Tail). 

%%----------------------------------------------------------------------
%% Function: createProcesses/5
%% Purpose: Starts seperate processes, each having their own ets
%% Args:   Number of processes left to spawn|BlankBoard|
%%         position of finish|Color of active robot|List of Process IDs
%% Returns: List of Process IDs to monitor 
%%----------------------------------------------------------------------
createProcesses(0,_,_,_,PidList)->
    PidList;
createProcesses(NumPro,BlankBoard,Finish,Color,PidList)->
	% Each process is assigned an ets to use
    AtomizedInt = list_to_atom(integer_to_list(NumPro-1)),
    Pid = spawn(ricochetAqueue,solveRecursive,[BlankBoard,Finish,Color,-1,AtomizedInt]),
    createProcesses(NumPro-1,BlankBoard,Finish,Color,[Pid|PidList]).

%%----------------------------------------------------------------------
%% Function: getBoardAndCycle/6
%% Purpose: Put the pieces on the board and begin the expansion
%% Args: BlankBoard|Active Robots Position|Ordered Position of Robots|
%%       Path so far| Depth currently before expansion| ETS 
%% Returns: true 
%%----------------------------------------------------------------------
getBoardAndCycle(BlankBoard, ActivePosition, AllPositions, Adventured, Depth, ETS)->
    FilledBoard = ricochetRobots:getFilledBoard(BlankBoard, AllPositions, []),
    cycle(FilledBoard,ActivePosition, AllPositions,Adventured,Depth,ETS).

%%----------------------------------------------------------------------
%% Function: cycle/6
%% Purpose: Make certain this board configuration doesnt already exists
%%          Add it to configurations checked and then expand by Moving each piece.
%%          Note that the swapping 2 non active robots yeilds the same configuration
%% Args: Board|Active Robots Position|Ordered Position of Robots|
%%       Path so far| Depth currently before expansion|ETS for this process
%% Returns: true 
%%----------------------------------------------------------------------
cycle(NewBoard,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth,ETS)->
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
				    move(NewBoard,Red,0,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth,ETS),
				    move(NewBoard,Yellow,1,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth,ETS),
				    move(NewBoard,Green,2,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth,ETS),
				    move(NewBoard,Blue,3,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth,ETS)
	    	end.

%%----------------------------------------------------------------------
%% Function: move/8
%% Purpose: Given a certain piece try to move it in any possible direction.
%%          Possible direction means there isnt a wall and if the most recent move 
%%          was this piece then it cant move in the oppsite direction it traveled
%% Args: Board|Position of Piece being moved|Color Value between 0 - 3|Active Robots Position|
%%       Ordered Position of Robots| Path so far| Depth currently before expansion| ETS 
%% Returns: true 
%%----------------------------------------------------------------------
move(Board,Position,CurrentColorValue,ActivesPosition,AllPositions,Adventured,Depth,ETS)->    
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
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,0,Adventured,Depth,ETS);
       	true -> northWall
    end,
    if 
    	CurrentValue rem 5 =/= 0, (PreviousDirection =/= 3 orelse CurrentColorValue =/= PreviousRobot)  -> % Robot Moving east
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,1,Adventured,Depth,ETS);
       	true -> eastWall
    end,
    if 
    	CurrentValue rem 7 =/= 0, (PreviousDirection =/= 0 orelse CurrentColorValue =/= PreviousRobot)  -> % Robot Moving south
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,2,Adventured,Depth,ETS);
       	true -> southWall
    end,
    if 
    	CurrentValue rem 11 =/= 0, (PreviousDirection =/= 1 orelse CurrentColorValue =/= PreviousRobot)  ->	% Robot Moving west
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,3,Adventured,Depth,ETS);
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
go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,Direction,Adventured,Depth,ETS) ->
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
		    ets:insert(ETS, {{FScore, erlang:phash2(now())},ActiveNewPosition, NewPositions, BinaryAdventured})
    end.

%%----------------------------------------------------------------------
%% Function: waitToFinish/3
%% Purpose: Solution has been found. Main process monitors returns solution once all processes end
%% Args: atom needed to force monitor|List of PIDs to monitor|Number of Processes
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
waitToFinish(add,PidList,NumPro) -> 
    lists:foreach(fun(X)->erlang:monitor(process,X) end, PidList),
    waitToFinish(NumPro,NumPro).
waitToFinish(0,OrigNumPro) ->
    {_Depth,Solution} = ets:lookup_element(solutions,found,2),
    Output = ricochetRobots:generateOutput(Solution),
	ricochetRobots:cleanUp(OrigNumPro),
	Output;
waitToFinish(NumPro,OrigNumPro) ->
    receive
		{'DOWN',_,_,_,_} ->
	    	waitToFinish(NumPro - 1,OrigNumPro)
    end.