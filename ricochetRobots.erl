-module(ricochetRobots).
-compile(export_all).

%%%--------------------------------------------------------------------- 
%%% Description module ricochetRobots
%%%--------------------------------------------------------------------- 
%%% This module has all the functions which are shared between the 
%%% various different search alogithms. This module alone reads in and
%%% converts the board to a computable list. Also the ricochet robots 
%%% heuristic is indepdently created in here. Other various functions,
%%% such as the function required to check a path to see how far a 
%%% robot can travel in a given direction, are located in here.
%%%--------------------------------------------------------------------- 

%%----------------------------------------------------------------------
%% Function: createBoard/1
%% Purpose: Intialize the all the ets', read in the board text file
%% Args:   textfile of the board
%% Returns: Board String
%%----------------------------------------------------------------------
createBoard(File) ->
	register(main,self()),
    ets:new(solutions, [set, named_table, public]),
    ets:new(configs, [ordered_set, named_table, public]),
    ets:new(minMoves, [ordered_set, named_table]),
    ets:new(open, [ordered_set, named_table, public]),
    ets:new(numProc, [set, named_table, public]),
    ricochetRobots:nodeCount(0),
    case file:read_file(File) of
		{ok, Board} -> 
	    	convert(erlang:binary_to_list(Board),[],0,0);
		{error, Reason} ->
	    	{error, Reason}
    end.
%%----------------------------------------------------------------------
%% Function: convert/4
%% Purpose: Converts Board into a list of {Row,Column,Character}, 32 x 32= 1024 Tuples
%% Args:   Board in Binary|Final Board|Row|Column
%% Returns: Converted Board
%%----------------------------------------------------------------------
convert(_Board,OrderedBoard,33,0) -> % Board fully converted
    B = lists:reverse(OrderedBoard),
    convertPrime(B,B,[]);
convert(Board,OrderedBoard,Row,Column) ->
    [CurrentChar|RemainingChars] = Board,
    case CurrentChar of
		13 -> 
			convert(RemainingChars,OrderedBoard,Row,Column); % 13 == "/r",just skip it
		10 -> 
			convert(RemainingChars,OrderedBoard,Row+1,0);	% 10 == "/n", New row means new tuple
		_  -> % Valid Character, ie. " " | - + R Y G B F
		    convert(RemainingChars,[{{Row,Column},CurrentChar}|OrderedBoard],Row,Column+1)
    end.

%%----------------------------------------------------------------------
%% Function: convert/4
%% Purpose: Converts List of 32 x 32 = 1024 into 16 x 16 = 512
%% 			Each of the tuples in th final list will have a specific integer
%% 			instead of a character. If the value is divisible by 3 then it has a 
%% 			wall to its north, if Value remainder 2 is zero then that position
%% 			is occupied by  piece. If the value is divisible by 17 then a yellow 
%% 			piece is there, etc.
%% Args:   Original Board|Board in List of Tuple Form|Final Board
%% Returns: Converted Final Board
%%----------------------------------------------------------------------
convertPrime(OriginalBoard, Board,ConvertedBoard) ->
    [{{R,C},V}|RestOfBoard]=Board,
    if  
    	R =:= 32 -> 
    		lists:reverse(ConvertedBoard); % Dont care about last row, DONE
		R rem 2 =:= 0 -> % Dont want even rows, skip
			convertPrime(OriginalBoard, RestOfBoard,ConvertedBoard);
		C rem 2 =:= 0 -> % Dont want even columns, skip
			convertPrime(OriginalBoard, RestOfBoard,ConvertedBoard);   
		true ->
		    {{_,_},N}=lists:keyfind({R-1,C},1,OriginalBoard), % N = character north of Current Character	
		    {{_,_},E}=lists:keyfind({R,C+1},1,OriginalBoard), % E = character east of Current Character
		    {{_,_},S}=lists:keyfind({R+1,C},1,OriginalBoard), % S = character south of Current Character
		    {{_,_},W}=lists:keyfind({R,C-1},1,OriginalBoard), % W = character west of Current Character
		    convertPrime(OriginalBoard, RestOfBoard, [{{(R+1) div 2,(C+1) div 2},mapValue(V,N,E,S,W)}|ConvertedBoard])
    end.

%%----------------------------------------------------------------------
%% Function: mapValue/5
%% Purpose: Multiplies a value by a prime for each property it has
%% Args:   Current Chacacter|Character To North| Character To east| 
%%		   Character To south| Character To west| 
%% Returns: integer whose prime factorization tells us about its neighbors and occupancy
%%----------------------------------------------------------------------
mapValue(V,N,E,S,W)->
    if 
    	N =:= 45 -> % 45 =:= -
    		Nvalue = 3; % Wall to the north
       	true->
       		Nvalue = 1
    end,
    if 
    	E =:= 124 -> % 124 =:= |
    		Evalue = 5; % Wall to the east
       	true->
       		Evalue = 1
    end,
    if 
    	S =:= 45 -> % 45 =:= -
    		Svalue = 7; % Wall to the stouh
       	true->
       		Svalue = 1
    end,
    if 
    	W =:= 124 -> % 124 =:= |
    		Wvalue = 11; % Wall to the west
       	true->
       		Wvalue = 1
    end,
    if 
    	V =:= 82 -> % 82 =:= R
    		Curvalue = 13*2; % Red, Occupied
       	V =:= 89 -> % 89 =:= Y
       		Curvalue = 17*2; % Yellow, Occupied
       	V =:= 71 -> % 71 =:= G
       		Curvalue = 19*2; % Green, Occupied
       	V =:= 66 -> % 66 =:= B
       		Curvalue = 23*2; % Blue, Occupied
       	V =:= 70 -> % 70 =:= F
       		Curvalue = 29; % Final/Finish
       	true->
       		Curvalue = 1
    end,
    Nvalue * Evalue * Svalue * Wvalue * Curvalue.

%%----------------------------------------------------------------------
%% Function: getValue/2
%% Purpose: Finds a positions value
%% Args:   Board|Position
%% Returns: Value at position
%%----------------------------------------------------------------------
getValue(Board, {X, Y}) ->
    {{_Row,_Col},V} = lists:nth(16*(X-1)+Y,Board),
    V.

%%----------------------------------------------------------------------
%% Function: convertSolution/2
%% Purpose: Adventured (the solution path) is in the unreadable form of [{2,1},{0,2},{3,0} ...]
%%			This is called once at the end to make it readable.
%% Args:   Initial Solution Path|Readable Path
%% Returns: Readable solution Path
%%----------------------------------------------------------------------
convertSolution([],Converted) ->
	lists:reverse(Converted); 
convertSolution([{ColorBit,DirectionBit}|Rest],Converted) ->
	Color = case ColorBit of 
				0 -> red;
				1 -> yellow;
				2 -> green;
				3 -> blue;
				_ -> invalidColor
	end,
	Direction = case DirectionBit of 
				0 -> north;
				1 -> east;
				2 -> south;
				3 -> west;
				_ -> invalidDirection
	end,
	convertSolution(Rest,[{Color,Direction}|Converted]).

%%----------------------------------------------------------------------
%% Function: getPosition/2
%% Purpose: Returns the Position of a given robot, or finish
%% Args:   Board|atom signifying request
%% Returns: {Row,Column}
%%----------------------------------------------------------------------
getPosition(Board, red) -> 		
	getPos(Board,13);
getPosition(Board, yellow) ->  	
	getPos(Board,17);
getPosition(Board, green) -> 	
	getPos(Board,19);
getPosition(Board, blue) ->  	
	getPos(Board,23);
getPosition(Board, finish) -> 	
	getPos(Board,29).
getPos(Board,X) -> 
	[Position] = [Pos||{Pos,Value} <- Board, Value rem X =:= 0],
	Position. 

%%----------------------------------------------------------------------
%% Function: getSmallest/2
%% Purpose: Finds the smallest scoring node in a given ordered_set ets. Since it is 
%%   	    ordered then the smallest is the first item. If no smallest
%%  		is found then either it tries again or returns empty
%% Args:   ETS| boolean, whether it is possible to return empty
%% Returns: Node with the smallest score
%%----------------------------------------------------------------------
getSmallest(Ets,Repeat) ->
    BestScoreKey = ets:first(Ets),
    if 
    	BestScoreKey =:= '$end_of_table' -> % ets is empty
	    	if 
	    		Repeat =:= true -> 
	    			getSmallest(Ets,Repeat);
	    		true -> 
	    			empty
	    	end;
	    true -> 
	    	NodeFound = ets:lookup(Ets, BestScoreKey),
       		case NodeFound of
       			[] -> 
       				% Node was deleted before we finally got to it
       				getSmallest(Ets,Repeat);
       			[{{FScore,_}, BestActivePosition, BestPositions, BestAdventured}] ->
				    ets:delete(Ets, BestScoreKey),
				    ricochetRobots:nodeCount(-1),
				    {{BestActivePosition, BestPositions, [{X,Y} || <<X:2,Y:2>> <= BestAdventured]},FScore};
				_ ->
					invalidNode
			end
    end.

%%----------------------------------------------------------------------
%% Function: getSmallest/3
%% Purpose: Same as function above but instead returns a list of all nodes with
%% 			the smallest score.
%% Args:   ETS| atom signifying to grab all lowest|Final List
%% Returns: List of Nodes with the smallest score
%%----------------------------------------------------------------------
getSmallest(Ets,all,[]) ->
    {{ActivePosition, AllPositions, Adventured},FScore} = getSmallest(Ets,true),
    getSmallest(Ets, FScore,[{ActivePosition, AllPositions, Adventured}]);
getSmallest(Ets,BestFScore,BestList) ->
    Smallest = getSmallest(Ets,false),
    if 
    	Smallest =/= empty -> % ets was empty
		    {{ActivePosition,Positions,Adventured},FScore} = Smallest,
		    if 
		    	FScore =:= BestFScore -> % Newly retrieved node does still have lowest score, add and get more		
			    	getSmallest(Ets, BestFScore, [{ActivePosition,Positions,Adventured}|BestList]);
		       	true ->% Newly retrieved node does NOT have lowest score, insert it back into the ETS
	       			BinaryAdventured = << <<C:2, D:2>> || {C,D} <- Adventured >>,
	    			ets:insert(open, {{FScore, erlang:phash2(now())},ActivePosition, Positions, BinaryAdventured}),
	    			{BestList,BestFScore}
		    end;	
      	true -> {BestList,BestFScore}
    end.

%%----------------------------------------------------------------------
%% Function: checkPath/3
%% Purpose: Given a position and direction this function finds the farthest a piece can move
%% Args:   Board| Position of Current Piece about to be moved|Direction Moving
%% Returns: Farthest position the piece can move to until it hits a wall
%%----------------------------------------------------------------------
checkPath(Board,{Row,Col},Direction)->
    case Direction of 
		0 -> % Get all Spots North of current position with a wall to the north OR occupied. First one found (first added to list) is closest. 
		    {{FutureRow,FutureCol},FutureValue} = lists:last([{{X,Y},V} || {{X,Y},V} <- Board , Y =:= Col, X < Row, V rem 3 =:= 0 orelse V rem 2 =:= 0]),
			% If the future position is occupied go to the spot directly south of it
		    if 
		    	FutureValue rem 2 =:= 0 ->
		    		{FutureRow+1,FutureCol};
		       	true -> 
		       		{FutureRow,FutureCol}
		    end;
		1  -> % Get all Spots East of current position with a wall to the east OR occupied. Last one found (current Head of list since last added) is closest. 
		    [{{FutureRow,FutureCol},FutureValue}|_Tail] = [{{X,Y},V} || {{X,Y},V} <- Board , X =:= Row, Y > Col, V rem 5 =:= 0 orelse V rem 2 =:= 0],
			% If the future position is occupied go to the spot directly west of it
		    if 
		    	FutureValue rem 2 =:= 0 -> 
		    		{FutureRow,FutureCol-1};
		       	true -> 
		       		{FutureRow,FutureCol}
		    end;
		2 -> % Get all Spots south of current position with a wall to the south OR occupied. Last one found is closest.  
		    [{{FutureRow,FutureCol},FutureValue}|_Tail] = [{{X,Y},V} || {{X,Y},V} <- Board , Y =:= Col, X > Row, V rem 7 =:= 0 orelse V rem 2 =:= 0],
		    % If the future position is occupied go to the spot directly north of it
		    if 
		    	FutureValue rem 2 =:= 0 -> 
		    		{FutureRow-1,FutureCol};
		       	true -> 
		       		{FutureRow,FutureCol}
		    end;
		3  -> % Get all Spots west of current position with a wall to the west OR occupied. First one found is closest.  
		    {{FutureRow,FutureCol},FutureValue} = lists:last([{{X,Y},V} || {{X,Y},V} <- Board , X =:= Row, Y < Col, V rem 11 =:= 0 orelse V rem 2 =:= 0]),
			% If the future position is occupied go to the spot directly east of it
		    if 
		    	FutureValue rem 2 =:= 0 -> 
		    		{FutureRow,FutureCol+1};
		       	true -> 
		       		{FutureRow,FutureCol}
		    end;
		_ -> 
			{error, invalidDirection}
    end.

%%----------------------------------------------------------------------
%% Function: idealCount/3
%% Purpose: Calculate the heuristic. The heuristic in this case is the
%%  		number of moves it would take for the active robot to move to
%%  		the finish position if it could behave as a rook, i.e. ignore 
%%  		other robots and turn whenever you wish
%% Args:   Board| Position of Final|Either breadthFirst or other
%% Returns: true
%%----------------------------------------------------------------------
idealCount(Board,Finish,Mode) ->
    lists:foreach(fun(X)->ets:insert(minMoves,{X,0}) end,lists:seq(1,256)),
	addToEts([Finish,{8,8},{8,9},{9,8},{9,9}],-1,[]),
    if 
    	% Since breadthFirst isnt allowed to use heuristic we dont calculate it
    	Mode =:= breadthFirst -> 
    		breadthFirst;
    	true -> 
    		minMovesRec(Board,[Finish],1)
	end,
	% Turn the finish spot back to a heuristic score of zero
	{X,Y} = Finish,
	[{Pos,_Value}] = ets:lookup(minMoves,16*(X-1)+Y),
	ets:insert(minMoves,{Pos,0}).

%%----------------------------------------------------------------------
%% Function: minMovesRec/3
%% Purpose: Helper function for idealCount,Finding minimum number of moves
%%          to get active robot to finish position
%% Args:   Board| Position of Final|Number of turns away from reaching finish position
%% Returns: done
%%----------------------------------------------------------------------
minMovesRec(Board,Finish,SpotsAway) ->
    MinMoves = ets:tab2list(minMoves),
    Remaining = [N || {_,N} <- MinMoves, N=:=0],
    if 
    	Remaining =:= [] -> % Base case is when no positions have a 0 left
    		done;
       	true -> 
       		NextToCheck = checkEach(Board,Finish,SpotsAway,[]),
	        minMovesRec(Board,NextToCheck,SpotsAway+1)
    end.

%%----------------------------------------------------------------------
%% Function: checkEach/4
%% Purpose: Helper function for idealCount,Finding minimum number of moves
%%          to get active robot to finish position
%% Args:   Board| Positions to check |Number of turns away from reaching finish position|
%%  	   List of positions who are + 1 more away
%% Returns: List of positions who are + 1 more away
%%----------------------------------------------------------------------
checkEach(_Board,[],_SpotsAway,Result) ->
	Result;
checkEach(Board,[Finish|Rest],SpotsAway,Result) ->
    NextCheck = checkPathMin(Board,Finish,SpotsAway),
    checkEach(Board,Rest,SpotsAway,NextCheck ++ Result).

%%----------------------------------------------------------------------
%% Function: checkPathMin/3
%% Purpose: Grabs all spots in every direction of initial spot
%% Args:   Board| Position of Current Piece about to be moved|Heuristic
%% Returns: List of positions who are + 1 more away
%%----------------------------------------------------------------------
checkPathMin(Board,{Row,Col},Acc)->
    CurrentValue = getValue(Board,{Row,Col}),
    if 
    	CurrentValue rem 3 =/= 0 -> % Get all Spots North of current position with a wall to the north OR occupied. First one found (first added to list) is closest. 
	    	{{EndRowN,_EndColN},_EndValueN} = lists:last([{{X,Y},V} || {{X,Y},V} <- Board , Y =:= Col, X < Row, V rem 3 =:= 0]),
	    	NorthSpots = [{R,C} || {{R,C},_V} <-Board, Col =:= C, R < Row, R >= EndRowN];
       	true -> 
       		NorthSpots = []
    end,
    if 
    	CurrentValue rem 5 =/= 0 -> % Get all Spots East of current position with a wall to the east OR occupied. Last one found (current Head of list since last added) is closest. 
	    	[{{_EndRowE,EndColE},_EndValueE}|_TailE] = [{{X,Y},V} || {{X,Y},V} <- Board , X =:= Row, Y > Col, V rem 5 =:= 0],
	    	EastSpots = [{R,C} || {{R,C},_V} <-Board, Row =:= R, C > Col, C =< EndColE];
       	true -> 
       		EastSpots = []
    end,
    if 
    	CurrentValue rem 7 =/= 0 -> % Get all Spots south of current position with a wall to the south OR occupied. Last one found is closest.  
	    	[{{EndRowS,_EndColS},_EndValueS}|_TailS] = [{{X,Y},V} || {{X,Y},V} <- Board , Y =:= Col, X > Row, V rem 7 =:= 0],
	    	SouthSpots = [{R,C} || {{R,C},_V} <-Board, Col =:= C, R > Row, R =< EndRowS];
       	true -> 
       		SouthSpots = []
    end,
    if 
    	CurrentValue rem 11 =/= 0 ->  % Get all Spots west of current position with a wall to the west OR occupied. First one found is closest.  
	    	{{_EndRowW,EndColW},_EndValueW} = lists:last([{{X,Y},V} || {{X,Y},V} <- Board , X =:= Row, Y < Col, V rem 11 =:= 0]),
	    	WestSpots = [{R,C} || {{R,C},_V} <-Board, Row =:= R, C < Col, C >= EndColW];
       	true -> 
       		WestSpots = []
    end,
    AllSpots = NorthSpots++EastSpots++SouthSpots++WestSpots,
    addToEts(AllSpots,Acc,[]).

%%----------------------------------------------------------------------
%% Function: addToEts/3
%% Purpose: Add values to the minMoves ets
%% Args:   List of Positions| Heuristic| List of those added
%% Returns: Next to check/List of those added
%%----------------------------------------------------------------------
addToEts([],_,Result) ->
	Result;
addToEts([{X,Y}|Tail],Acc,Result)->
    [{Pos,Value}] = ets:lookup(minMoves,16*(X-1)+Y),
    if 
    	Value =/= 0 -> 
    		addToEts(Tail,Acc,Result);
       	true -> 
       		ets:insert(minMoves,{Pos,Acc}),
       		addToEts(Tail,Acc,[{X,Y}|Result])
    end.

%%----------------------------------------------------------------------
%% Function: getFilledBoard/3
%% Purpose: Alters the values of the positions which now have robots on them
%% Args:  	Board| Ordered Position of robots,| Filled Board
%% Returns: Filled Board
%%----------------------------------------------------------------------
getFilledBoard([],_AllPositions,FilledBoard) ->
	lists:reverse(FilledBoard);
getFilledBoard([{Pos,V}|RestBoard],[{Red},{Yellow},{Green},{Blue}],FilledBoard) ->
    if 
    	Pos =:= Red ->
	    	getFilledBoard(RestBoard,[{Red},{Yellow},{Green},{Blue}],[{Pos,V * 26}|FilledBoard]);
       	Pos =:= Yellow ->
	    	getFilledBoard(RestBoard,[{Red},{Yellow},{Green},{Blue}],[{Pos,V * 34}|FilledBoard]);
       	Pos =:= Green ->
	    	getFilledBoard(RestBoard,[{Red},{Yellow},{Green},{Blue}],[{Pos,V * 38}|FilledBoard]);
       	Pos =:= Blue ->
	    	getFilledBoard(RestBoard,[{Red},{Yellow},{Green},{Blue}],[{Pos,V * 46}|FilledBoard]);
       	true ->
	    	getFilledBoard(RestBoard,[{Red},{Yellow},{Green},{Blue}],[{Pos,V}|FilledBoard])
    end.  

%%----------------------------------------------------------------------
%% Function: getBlankBoard/3
%% Purpose: Alters the values of the positions which had have robots on them
%% Args:  	Board| Filled Board
%% Returns: Filled Board
%%----------------------------------------------------------------------
getBlankBoard([],BlankBoard) ->
	lists:reverse(BlankBoard);
getBlankBoard([{{Row,Col},V}|RestBoard],BlankBoard) ->
    if 
    	V rem 13 =:= 0 ->
	    	getBlankBoard(RestBoard,[{{Row,Col},V div 26}|BlankBoard]);
       	V rem 17 =:= 0 ->
	    	getBlankBoard(RestBoard,[{{Row,Col},V div 34}|BlankBoard]);
       	V rem 19 =:= 0 ->
	    	getBlankBoard(RestBoard,[{{Row,Col},V div 38}|BlankBoard]);
       	V rem 23 =:= 0 ->
	    	getBlankBoard(RestBoard,[{{Row,Col},V div 46}|BlankBoard]);
       	true ->
	    	getBlankBoard(RestBoard,[{{Row,Col},V}|BlankBoard])
    end.

%%----------------------------------------------------------------------
%% Function: nodeCount/1
%% Purpose: Increments or decrements the number of total nodes, also
%%	 	    can initialize or return values
%% Args:  	Number to alter by, or atom to get counts
%% Returns: Number of Nodes
%%----------------------------------------------------------------------
nodeCount(N) ->
	if
		N =:= 1 -> % Incremenets total node count by N
	    	ets:insert(solutions, {nodesCreated, ets:lookup_element(solutions,nodesCreated,2) + N});
		N =:= -1 -> % Decrements total node count by N
	    	ets:insert(solutions, {nodesDeleted,ets:lookup_element(solutions,nodesDeleted,2) - N});
		N =:= 0 -> %intializes node counts
		    ets:insert(solutions, {nodesCreated, 0}),		% Used for A* to give unique keys to nodes inserted into open ets
		    ets:insert(solutions, {nodesDeleted, 0});
		N =:= getCreated -> 
			ets:lookup_element(solutions,nodesCreated,2);
		true -> % total number of nodes present
			ets:lookup_element(solutions,nodesCreated,2) - ets:lookup_element(solutions,nodesDeleted,2)
	end.

%%----------------------------------------------------------------------
%% Function: generateOutput/1
%% Purpose: returns tuple of search findings
%% Args:  	Unconverted Path Solution
%% Returns: {Time,Number of Nodes, Converted Solution Path}
%%----------------------------------------------------------------------
generateOutput(Solution) ->
	{StartMega, StartSec, StartMicro} = ets:lookup_element(solutions,time,2),
    {CurMega, CurSec, CurMicro} = now(),
    NumNodes = ricochetRobots:nodeCount(getCreated), 
    {{CurMega-StartMega,CurSec-StartSec,CurMicro-StartMicro},NumNodes,
    convertSolution(Solution,[])}.

%%----------------------------------------------------------------------
%% Function: cleanUp/1
%% Purpose: Delete all ETS' used in the search, and unregister pid
%% Args:  	aqueue and bqueue make this many more ETS',leave 0 if not that searchtype 
%% Returns: {Time,Number of Nodes, Converted Solution Path}
%%----------------------------------------------------------------------
cleanUp(ExtraETS) ->
	if 
		ExtraETS > 0 ->
    		lists:foreach(fun(N) -> ets:delete(erlang:list_to_atom(integer_to_list(N))) end, lists:seq(0,ExtraETS -1));
    	true-> noneToDelete
	end,
	ets:delete(solutions),
    ets:delete(configs),
    ets:delete(minMoves),
    ets:delete(open),
    ets:delete(numProc),
	unregister(main).