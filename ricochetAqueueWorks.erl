
-module(ricochetAqueue).
-compile([debug_info, export_all]).

%% Color is the piece desired to get to the Final position
solve(Board, Color, NumPro, Mode) ->
    ets:insert(solutions, {time, now()}), 	% Timer starts
    BlankBoard = ricochetRobots:getBlankBoard(Board,[]),		% Used for A* so filled boards are not stored in open ets
    Finish = ricochetRobots:getPosition(Board,finish),		
    ricochetRobots:idealCount(Board,Finish,Mode),				% Calculates "Rook" moves = minimum number of moves to get active(Color) to finish(final)
    AllPositions = [{ricochetRobots:getPosition(Board,red)},{ricochetRobots:getPosition(Board,yellow)},
    				{ricochetRobots:getPosition(Board,green)},{ricochetRobots:getPosition(Board,blue)}],
    cycle(Board,ricochetRobots:getPosition(Board,Color),AllPositions,Finish,[],0,open), % Non recursive call to move each piece in each direction once
    solveRecursive(BlankBoard,Finish,Color,NumPro,open). % !! MUST BE UNDER 2034 !!

solveRecursive(BlankBoard,Finish,Color,NumPro,AtomizedInt)->
    NumNodes = ricochetRobots:nodeCount(get),
    if 
    	NumPro > 0 andalso NumNodes >= NumPro -> % Only true the first time we have at least NumPro number of nodes
		    fillETS(NumPro,0,ets:tab2list(AtomizedInt)), 
		    PidList = createProcesses(NumPro,BlankBoard,Finish,Color,[]),
		    waitToFinish(add,PidList,NumPro);
    	true->     
		    Smallest = ricochetRobots:getSmallest(AtomizedInt,false),
		    if 
		    	Smallest =:= empty -> 
		    		AtomizedInt;
		    	true -> 
				    {{{X,Y}, AllPositions, Adventured},FScore} = Smallest,	% {X,Y} is position of active robot
				    [{_,MinMoves}] = ets:lookup(minMoves,16*(X-1)+Y),		% Heuristic used in A*
				    Depth = FScore - MinMoves,
				    Solutions = ets:lookup(solutions,found),
				    case Solutions of
						[] -> % no solution exists, try again													
						    getBoardAndCycle(BlankBoard,{X,Y},AllPositions,Finish,Adventured,Depth,AtomizedInt),
						    solveRecursive(BlankBoard,Finish,Color,NumPro,AtomizedInt);
						_ -> % SOME process found a solution but this process neds to "catch up" since its still
							 % possible it can find a better solution
						    [{found,{SolDepth,_}}] = Solutions,
						    if 
						    	FScore =< SolDepth -> % Possible for a better solution to be found
								    getBoardAndCycle(BlankBoard,{X,Y},AllPositions,Finish,Adventured,Depth,AtomizedInt),
								    solveRecursive(BlankBoard,Finish,Color,NumPro,AtomizedInt);
						       	true -> % SOME process found a solution AND this process cant do possibly find a better one
								     if 
								     	NumPro > 0 -> % a solution was found BEFORE we can get numpro number of nodes
								     		waitToFinish(0,0); % Will properly end the the program
								     	true -> 
								     		done % This process/ this ets's job is done
								     end
						    end
				    end
		    end
    end.

createProcesses(0,_,_,_,PidList)->
    PidList;
createProcesses(NumPro,BlankBoard,Finish,Color,PidList)->
    AtomizedInt = list_to_atom(integer_to_list(NumPro-1)),
    Pid = spawn(ricochetAqueue,solveRecursive,[BlankBoard,Finish,Color,-1,AtomizedInt]),
    createProcesses(NumPro-1,BlankBoard,Finish,Color,[Pid|PidList]).

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

getBoardAndCycle(BlankBoard, ActivePosition, AllPositions, Finish, Adventured, Depth,AtomizedInt)->
    FilledBoard = ricochetRobots:getFilledBoard(BlankBoard, AllPositions, []),	% Fills board with all positions
    cycle(FilledBoard,ActivePosition, AllPositions, Finish,Adventured,Depth,AtomizedInt).

cycle(NewBoard,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Finish,Adventured,Depth,AtomizedInt)->
    if 
    	ActivesPosition =:= Finish -> % robot is at final destination
		    OldSol = ets:lookup(solutions, found),
		    case OldSol of
				[]->
				    ets:insert(solutions, {found, {Depth - 1,lists:reverse(Adventured)}});
				_->
				    [{found,{OldDepth,_}}] = OldSol,
				    if 
				    	OldDepth > Depth - 1 ->
					    	ets:insert(solutions, {found, {Depth - 1,lists:reverse(Adventured)}});
				       	true ->
					    	done
				    end
		    end;
      	true->
		    ThreeNonActives = [Red,Yellow,Green,Blue]--[ActivesPosition],
		    [NonActive1,NonActive2,NonActive3] = lists:sort(ThreeNonActives),
		    HashValue = erlang:phash2({ActivesPosition,NonActive1,NonActive2,NonActive3}),
		    MatchingList = ets:lookup(configs,HashValue),
		    if 
		    	MatchingList =:= [] -> 
		    		DepthDone = infinity; %arbitrary large number
		       	true ->
		       		[{_,DepthDone}] = MatchingList
		    end,
		    if 
		    	DepthDone =< Depth -> 
		    		beenDone;
		       	true ->
				    ets:insert(configs, {HashValue, Depth}),
				    move(NewBoard,Red,0,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth,AtomizedInt),
				    move(NewBoard,Yellow,1,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth,AtomizedInt),
				    move(NewBoard,Green,2,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth,AtomizedInt),
				    move(NewBoard,Blue,3,ActivesPosition,[{Red},{Yellow},{Green},{Blue}],Adventured,Depth,AtomizedInt)
	    	end
	end.

move(Board,Position,CurrentColorValue,ActivesPosition,AllPositions,Adventured,Depth,AtomizedInt)->    
	case Adventured of
		[] -> 
			PreviousRobot = undefined,
			PreviousDirection = undefined;
    	_ ->
    		[{PreviousRobot,PreviousDirection} | _Rest] = Adventured
	end,
    CurrentValue = ricochetRobots:getValue(Board,Position), 
    if 
    	CurrentValue rem 3 =/= 0, (PreviousDirection =/= 2 orelse CurrentColorValue =/= PreviousRobot) -> % Robot Moving north	
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,0,Adventured,Depth,AtomizedInt);
       	true -> northWall
    end,
    if 
    	CurrentValue rem 5 =/= 0, (PreviousDirection =/= 3 orelse CurrentColorValue =/= PreviousRobot)  -> % Robot Moving east
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,1,Adventured,Depth,AtomizedInt);
       	true -> eastWall
    end,
    if 
    	CurrentValue rem 7 =/= 0, (PreviousDirection =/= 0 orelse CurrentColorValue =/= PreviousRobot)  -> % Robot Moving south
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,2,Adventured,Depth,AtomizedInt);
       	true -> southWall
    end,
    if 
    	CurrentValue rem 11 =/= 0, (PreviousDirection =/= 1 orelse CurrentColorValue =/= PreviousRobot)  ->	% Robot Moving west
	    	go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,3,Adventured,Depth,AtomizedInt);
       	true -> westWall
    end.

%% Updates the Board with the robot at its new position
go(Board,ActivesPosition, Position,AllPositions,CurrentColorValue,Direction,Adventured,Depth,AtomizedInt) ->
	{Row,Col} = ricochetRobots:checkPath(Board,Position,Direction),
    if 
    	Position =:= {Row,Col} -> %Robot trying to move to current location means an occupied position is 1 spot away
			blocked;
    	true-> 
			NewPositions = lists:keyreplace(Position,1,AllPositions,{{Row,Col}}),
		    if 
		    	Position =:= ActivesPosition ->
			    	ActiveNewPosition = {Row,Col};
		       	true ->
			    	ActiveNewPosition = ActivesPosition
		    end,
		    {ActiveRow, ActiveCol} = ActiveNewPosition,
		    [{_,MinMoves}] = ets:lookup(minMoves,16*(ActiveRow-1)+ActiveCol),
		    FScore = (Depth + 1) + MinMoves,
		    ricochetRobots:nodeCount(1),
		    BinaryAdventured = << <<C:2, D:2>> || {C,D} <- [{CurrentColorValue, Direction}|Adventured] >>,
		    ets:insert(AtomizedInt, {{FScore, erlang:phash2(now())},ActiveNewPosition, NewPositions, BinaryAdventured})
    end.

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