-module(printData).
-compile(export_all).

%%%--------------------------------------------------------------------- 
%%% Description module printData
%%%--------------------------------------------------------------------- 
%%% This module contains the only functions that should be executed by
%%% the user. This includes solving just once, or many times in various ways
%%%--------------------------------------------------------------------- 

%% cd("C:/erlang/"). c(ricochetIDlevel). c(ricochetIDproc). c(ricochetA). c(ricochetAlevel). c(ricochetAproc). c(ricochetAqueue). c(printData). c(ricochetRobots). 
%  cd("C:/erlang/"). c(ricochetRobots). c(ricochetP). c(printData). printData:solve(id,3,"boardSimple.txt",red).
%  cd("C:/erlang/"). c(ricochetAqueue). c(printData). printData:compareCores([alevel],[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],"r12y13g15b13.txt",green).
%  cd("C:/erlang/"). c(ricochetAqueue). c(printData). printData:compare(all,"boardSimple.txt",blue).
%  cd("C:/erlang/"). c(ricochetAqueue). c(printData). printData:findBest(aqueue,"boardSimple.txt",red,1,100,10).
%  cd("C:/erlang/"). c(ricochetAPP). c(printData). printData:printProc(aproc,"boardSimple.txt",red,4,5,5).
%  cd("C:/erlang/"). c(ricochetAPP). c(printData). printData:printProc(aproc,"r12y13g15b13.txt",green,21,24,5).

%%----------------------------------------------------------------------
%% Function: solve/4
%% Purpose: Will solve the given board using the given color and retun a tuple of 
%% 			the time it took, number of nodes created, and solution. If process count can be specified
%% 			then it will be taken from NumPro
%% Args:   SearchType|Number of Processes, Board of 16 x 16 2-tuples|Color of active robot
%% Returns: {Time, Number of Nodes, Solution} 
%%----------------------------------------------------------------------
solve(SearchType,NumPro,BoardFile,Color) ->
 
	Board = ricochetRobots:createBoard(BoardFile),
	{Time,Nodes,Path} = case SearchType of 
		id ->
		    ricochetIDlevel:solve(Board,Color,0);
		idlevel ->
		    ricochetIDlevel:solve(Board,Color,NumPro);
		idproc ->
		    ricochetIDproc:solve(Board,Color,NumPro);
		a -> 
			ricochetA:solve(Board,Color,astar);
		alevel -> 
			ricochetAlevel:solve(Board,Color,astar);
		aproc -> 
			ricochetAproc:solve(Board,Color,NumPro,astar);
		aqueue ->
			ricochetAqueue:solve(Board,Color,NumPro,astar);
		b -> 
			ricochetA:solve(Board,Color,breadthFirst);
		blevel ->
			ricochetAlevel:solve(Board,Color,breadthFirst);
		bproc -> 
			ricochetAproc:solve(Board,Color,NumPro,breadthFirst);
		bqueue ->
			ricochetAqueue:solve(Board,Color,NumPro,breadthFirst);
		_ -> 
			{{0,0,0},invalidSearchType}
    end,
    {tupleToSeconds(Time),Nodes,Path}.

%%----------------------------------------------------------------------
%% Function: tupleToMicro/3
%% Purpose: Converts the time to integer of microseconds
%% Args:   Time Tuple
%% Returns: Number in MicroSeconds
%%----------------------------------------------------------------------
tupleToMicro({Macro,Sec,Micro}) ->
	MacroToSeconds = Macro * 1000000,
	(MacroToSeconds + Sec) * 1000000 + Micro.

%%----------------------------------------------------------------------
%% Function: tupleToSeconds/3
%% Purpose: Converts the time to integer of seconds
%% Args:   Time Tuple
%% Returns: Number in Seconds
%%----------------------------------------------------------------------
tupleToSeconds({Macro,Sec,Micro}) ->
	MacroToSeconds = Macro * 1000000,
	(MacroToSeconds + Sec) + (0.000001 * Micro).

%%----------------------------------------------------------------------
%% Function: createFile/1
%% Purpose: Will print to a file contain system information, returning filename
%% Args:   Color of active robot
%% Returns: filename
%%----------------------------------------------------------------------
createFile(Color) -> 
	{Year,Month,Day} = date(),
	{Hour,Minute,Second} = time(),
	Cores = erlang:system_info(logical_processors_available),
	OS = erlang:system_info(system_architecture),
	Name = io_lib:fwrite("~p"++OS++"~p~p~p-~p.~p.~pcompare(~p).txt",[Cores,Month,Day,Year,Hour,Minute,Second,Color]),
	Folder = "./output/",
	FileName = Folder ++ Name,
	case % Create Folder if it doesnt exist
		file:make_dir(Folder) of
			{error, _Reason} -> 
				dirExists;
			_ -> 
				dirMade
	end,
	%application:start(sasl),application:start(os_mon),
	%[{total_memory,TotalMemory},{free_memory,FreeMemory},{system_total_memory,SysTotalMemory}] = memsup:get_system_memory_data(),
	%[{processor,_CPUDetected}] = erlang:system_info({cpu_topology, detected}),
	%[{processor,CPUUsed}] = erlang:system_info({cpu_topology, used}),
	%Proc = erlang:system_info(logical_processors),
	ProcAvailable = erlang:system_info(logical_processors_available),
	SystemVersion = erlang:system_info(system_version),
	ErlangDriverVersion = erlang:system_info(driver_version),
	file:write_file(FileName, SystemVersion),
	file:write_file(FileName, "Erlang Driver Version :"++ErlangDriverVersion++" \n",[append]),
	file:write_file(FileName, io_lib:fwrite("Total Memory: ~p \n Free Memory: ~p \n System Total Memory: ~p \n",[undefined,undefined,undefined]),[append]),
	%file:write_file(FileName, io_lib:fwrite("Procs Available: ~p \n CPU Used : ~p \n",[ProcAvailable,CPUUsed]),[append]),
	FileName.

%%----------------------------------------------------------------------
%% Function: printProc/6
%% Purpose: Will print to a file the results of using SearchType
%%			SearchType will be executed with LowerBound thru UpperBound
%%  		number of processes, incrementing by Increment
%% Args:   Search Type| board textfile| Color of active Robot| Lowest amount of process to try|
%%		   Highest amount of processes to try| Process count to increment by 
%% Returns: file information 
%%----------------------------------------------------------------------
printProc(SearchType,BoardFile,Color,LowerBound,UpperBound,Increment) ->
	FileName = createFile(Color),
	printProc(SearchType,BoardFile,Color,LowerBound,UpperBound,Increment,FileName).
printProc(SearchType,BoardFile,Color,LowerBound,UpperBound,Increment,FileName) ->
	if 
		LowerBound > UpperBound -> % done
			file:read_file_info(FileName); 
		true -> 
			{Time,Nodes,Path} = solve(SearchType,LowerBound,BoardFile,Color),
			% Length of Solution Found
			PathCount = lists:foldl(fun(_X,Sum) -> Sum + 1  end, 0, Path),
			file:write_file(FileName, io_lib:fwrite("Search Type :~p: Proc :~p: Time :~p: Nodes :~p: Color(~p) :~p:\n", [SearchType,LowerBound,Time,Nodes,Color,PathCount]),[append]),
			printProc(SearchType,BoardFile,Color,LowerBound + Increment ,UpperBound,Increment,FileName)
	end.

%%----------------------------------------------------------------------
%% Function: findBest/6
%% Purpose: Finds the ideal number of processes. Will search given intervals and continually 
%%          get more percise. For example if LowerBound is 1 and UpperBound is 1000, with an interval of 10
%%			then there will be a search at 1 process, 10 process, 20, 30, ... 990, 1000. If 50 yeilded the best
%%          score then a search from 40 thru 60 will occuring starting with a search of 40 process, then 
%%			42,44,... 58, 60, because we sepcified 10 intervals in the beggining. If 46 then yeilded the best result
%% 			we would return the best of either 44,45,46,47,48.
%% Args:   Search Type| board textfile| Color of active Robot| Lowest amount of process to try|
%%		   Highest amount of processes to try| Amount of searches between bounds
%% Returns: {Best Number of Processes, Time at that Process count}
%%----------------------------------------------------------------------
findBest(SearchType,BoardFile,Color,LowerBound,UpperBound,Intervals) ->
	Increment = (UpperBound - LowerBound) / Intervals,
	if 
		(UpperBound - LowerBound) =< Intervals -> 
			% The interval count is higher than difference of bounds, try all inbetween bounds
			continueFindBest(SearchType,BoardFile,Color,erlang:trunc(LowerBound),erlang:trunc(UpperBound)+1,1,{0,infinity});
		true -> 
			{BestNumProc,_BestTime} = continueFindBest(SearchType,BoardFile,Color,LowerBound,UpperBound,Increment,{0,infinity}),
			findBest(SearchType,BoardFile,Color,BestNumProc - Increment,BestNumProc + Increment,Intervals)
	end.
continueFindBest(SearchType,BoardFile,Color,LowerBound,UpperBound,Increment,{BestNumProc,BestTime}) -> 
	if 
		LowerBound > UpperBound -> % done
			{BestNumProc,BestTime};
		true ->  
			if % incase NumProc drops below 1
				erlang:round(LowerBound) < 1 -> 
					NextNum = 1;
				true -> 
					NextNum = erlang:round(LowerBound)
			end,
			{Time,_Nodes,_Path} = solve(SearchType,NextNum,BoardFile,Color),
			if 
				Time >= BestTime -> % Newest Search was not better, try again
					continueFindBest(SearchType,BoardFile,Color,LowerBound + Increment,UpperBound,Increment,{BestNumProc,BestTime});
				true -> % Newest search yielded better time
					continueFindBest(SearchType,BoardFile,Color,LowerBound + Increment,UpperBound,Increment,{LowerBound,Time})
			end
	end.

%%----------------------------------------------------------------------
%% Function: compare/3
%% Purpose: Compares various different search types, if a number of processes is 
%%          but not specified then findBest is called to calculate the best  
%% Args:   Search Type| board textfile| Color of active Robot
%% Returns: file information
%%----------------------------------------------------------------------
compare(SearchTypes,BoardFile,Color) -> 
	FileName = createFile(Color),
	comparePrint(SearchTypes,BoardFile,Color,FileName).
	comparePrint([],_,_,FileName) -> file:read_file_info(FileName);
comparePrint(all,BoardFile,Color,FileName) -> 
	% If searchtype is all then compare all searchtypes
	comparePrint([id,idlevel,idproc,a,alevel,aproc,aqueue,b,blevel,bproc,bqueue],BoardFile,Color,FileName);
comparePrint([{SearchType,NumPro}|Rest],BoardFile,Color,FileName) ->
	{Time,Nodes,Path} =	case SearchType of
		id -> 
			Proc = undefined, 
			solve(SearchType,undefined,BoardFile,Color);
		a -> 
			Proc = undefined, 
			solve(SearchType,undefined,BoardFile,Color);
		alevel -> 
			Proc = undefined, 
			solve(SearchType,undefined,BoardFile,Color);
		b -> 
			Proc = undefined, 
			solve(SearchType,undefined,BoardFile,Color);
		blevel -> 
			Proc = undefined, 
			solve(SearchType,undefined,BoardFile,Color);
		_ -> 
			if 
				NumPro =:= undefined -> 
					{BestNumPro,_BestTime} = findBest(SearchType,BoardFile,Color,1,100,5),
					Proc = BestNumPro, 
					solve(SearchType,BestNumPro,BoardFile,Color);
				true -> 
					Proc = NumPro, 
					solve(SearchType,NumPro,BoardFile,Color)
			end
	end,
	PathCount = lists:foldl(fun(_X,Sum) -> Sum + 1  end, 0, Path),
	%file:write_file(FileName, io_lib:fwrite("Search Type :~p: Proc :~p: Time :~p: Nodes :~p: Color(~p) :~p:\n", [SearchType,Proc,Time,Nodes,Color,PathCount]),[append]),
	file:write_file(FileName, io_lib:fwrite("~p\t~p\t~p\t~p\t~p\t~p\n", [SearchType,Proc,Time,Nodes,PathCount,Path]),[append]),
	comparePrint(Rest,BoardFile,Color,FileName);
comparePrint(SearchType,BoardFile,Color,FileName) ->
	% since input was not a tuple of {searchtype, process count} then it will make tuples
	ToTuple = [{Type,undefined} || Type <- SearchType],
	comparePrint(ToTuple,BoardFile,Color,FileName).


compareCores(SearchTypes,Cores,BoardFile,Color) -> 
	FileName = createFile(Color),	
	compareCores(SearchTypes,Cores,BoardFile,Color,FileName).
compareCores(SearchTypes,[FirstCoreCount|Rest],BoardFile,Color,FileName) ->
	erlang:system_flag(schedulers_online,FirstCoreCount),
	file:write_file(FileName, io_lib:fwrite("~p\t", [FirstCoreCount]),[append]),
	comparePrint(SearchTypes,BoardFile,Color,FileName),
	compareCores(SearchTypes,Rest,BoardFile,Color,FileName);
compareCores(_,[],_,_,FileName) ->
	file:read_file_info(FileName).
