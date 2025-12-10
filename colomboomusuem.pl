:- discontiguous edges/3.

% Rooms in the museum (ground floor)
room(entrance_checking).
room(police_room).
room(taxidermists_room).
room(mineral_gallery).
room(store).
room(center_hall).
room(tombstones).
room(medival_gallery).
room(stone_gallery).
room(inscriptions).
room(office).
room(cannon).
room(office2).
room(library).
room(reading_room).
room(exit1).

% Upper floor
room(entrance).
room(staircase_up).
room(sambur_group).
room(deer_group).
room(dugong).
room(elephant).
room(giant_tortoise).
room(basking_shark).
room(exit2).

% Define edges with distances (feet)
% Ground Floor 
edges(entrance_checking, office, 1).
edges(office, stone_gallery, 10).
edges(stone_gallery, inscriptions, 5).
edges(stone_gallery, cannon, 10).
edges(stone_gallery, tombstones, 10).
edges(stone_gallery, medival_gallery, 10).
edges(tombstones, medival_gallery, 5).
edges(tombstones, center_hall, 10).
edges(tombstones, mineral_gallery, 20).
edges(tombstones, store, 20).
edges(tombstones, taxidermists_room, 25).
edges(medival_gallery, cannon, 5).
edges(medival_gallery, center_hall, 10).
edges(cannon, center_hall, 10).
edges(center_hall, office2, 10).
edges(center_hall, library, 15).
edges(center_hall, mineral_gallery, 20).
edges(center_hall, store, 20).
edges(center_hall, taxidermists_room, 25).
edges(center_hall, reading_room, 25).
edges(mineral_gallery, store, 5).
edges(mineral_gallery, taxidermists_room, 10).
edges(store, taxidermists_room, 10).
edges(office2, library, 5).
edges(office2, reading_room, 10).
edges(library, reading_room, 10).
edges(entrance_checking, police_room, 1).
edges(reading_room, exit1, 5).

% Blocked Road
blocked(office2,library).
blocked(medival_gallery,cannon).

% Upper Floor 

edges(center_hall, staircase_up, 10).
edges(tombstones, staircase_up, 10).
edges(office2, staircase_up, 10).
edges(staircase_up, entrance, 5).
edges(entrance, deer_group, 1).
edges(entrance, sambur_group, 5).
edges(deer_group, sambur_group, 5).
edges(sambur_group, giant_tortoise, 10).
edges(sambur_group, basking_shark, 15).
edges(sambur_group, elephant, 15).
edges(sambur_group, dugong, 15).
edges(giant_tortoise, basking_shark, 5).
edges(elephant, dugong, 5).
edges(basking_shark, exit2, 5).

% Bidirectional connections

connected(X, Y, D) :- edges(X, Y, D),\+ blocked(X,Y).
connected(Y, X, D) :- edges(X, Y, D),\+ blocked(Y,X).

% Heuristic min distance to exit number "2"

h(exit2,entrance_checking,8).
h(exit2,police_room,9).
h(exit2,taxidermists_room,8).
h(exit2,mineral_gallery,7).
h(exit2,store,7).
h(exit2,center_hall,6).
h(exit2,tombstones,5).
h(exit2,medival_gallery,6).
h(exit2,cannon,7).
h(exit2,stone_gallery,7).
h(exit2,inscriptions,8).
h(exit2,office,7).
h(exit2,office2,5).
h(exit2,library,6).
h(exit2,reading_room,7).
h(exit2,exit1,7).
h(exit2,staircase_up, 5).
h(exit2,entrance,4).
h(exit2,deer_group,3).
h(exit2,sambur_group,2).
h(exit2,giant_tortoise,5).
h(exit2,basking_shark,6).
h(exit2,dugong,6).
h(exit2,elephant,6).
h(exit2,exit2,0).

% Heuristic min distance to exit number "1"
h(exit1,entrance_checking, 6).
h(exit1,police_room, 7).
h(exit1,taxidermists_room, 6).
h(exit1,mineral_gallery, 5).
h(exit1,store, 4).
h(exit1,center_hall, 3).
h(exit1,tombstones, 4).
h(exit1,medival_gallery, 5).
h(exit1,stone_gallery, 5).
h(exit1,inscriptions, 6).
h(exit1,office, 7).
h(exit1,cannon, 6).
h(exit1,office2, 2).
h(exit1,library, 2).
h(exit1,reading_room, 1).
h(exit1,exit1, 0).
h(exit1,entrance, 5).
h(exit1,staircase_up, 3).
h(exit1,sambur_group, 6).
h(exit1,deer_group, 7).
h(exit1,dugong, 8).
h(exit1,elephant, 8).
h(exit1,giant_tortoise, 8).
h(exit1,basking_shark, 8).
h(exit1,exit2, 7).

% BFS to find shortest path (by number of hops)

bfs(Start, Goal, Path) :-
    room(Start), room(Goal),
    bfs_queue([[Start]], Goal, RevPath),
    reverse(RevPath, Path).

bfs_queue([[Goal | Rest] | _], Goal, [Goal | Rest]).

bfs_queue([[Current | Rest] | Other], Goal, Path) :-
    findall([Next, Current | Rest],
            (connected(Current, Next, _),
             \+ member(Next, [Current | Rest])),
            NewPaths),
    append(Other, NewPaths, Updated),
    bfs_queue(Updated, Goal, Path).	

% DFS to find shortest path (by number of hops)	

dfs(Start, Goal, Path) :-
    room(Start), room(Goal),
    dfs_queue([[Start]], Goal, RevPath),
    reverse(RevPath, Path).

dfs_queue([[Goal | Rest] | _], Goal, [Goal | Rest]).

dfs_queue([[Current | Rest] | Other], Goal, Path) :-
    findall([Next, Current | Rest],
            (connected(Current, Next, _),
             \+ member(Next, [Current | Rest])),
            NewPaths),
    append(NewPaths, Other, Updated),
    dfs_queue(Updated, Goal, Path).

% Calculate the total path cost 

pathcost([],0). % Empty list
pathcost([_],0). % single room
pathcost([X,Y|T],Cost):- 
    connected(X,Y,D),
    pathcost([Y|T],Rest),
    Cost is D + Rest.
            
all_bfs_paths(Start,Goal,Paths):-
    findall(Path,bfs(Start, Goal, Path),Paths).
	
all_dfs_paths(Start,Goal,Paths):-
    findall(Path,dfs(Start, Goal, Path),Paths).	

paths_with_costs(Paths,Pairs):-
    findall(Cost-Path,
            (member(Path,Paths),pathcost(Path,Cost)),
            Pairs).

shortest_path(Paths,ShortestPath,MinCost):-
    paths_with_costs(Paths,Pairs),
    keysort(Pairs,[MinCost-ShortestPath|_]).

all_bfs_path_cost(Start,Goal,Path,Cost):-
    all_bfs_paths(Start,Goal,Paths),
    shortest_path(Paths,Path,Cost).

all_dfs_path_cost(Start,Goal,Path,Cost):-
    all_dfs_paths(Start,Goal,Paths),
    shortest_path(Paths,Path,Cost).

% A* Search Algorithm

astar(Start,Goal,Path,Cost):-
    room(Start),room(Goal),
    h(Goal,Start,H0),
    astar_search([node(Start,[Start],0,H0)],Goal,RevPath,Cost),
    reverse(RevPath,Path). % reversing the reverse path
	
astar_search([node(State,RevPath,G,_)|_],Goal,RevPath,G):-
    State=Goal,!.  % we reached the goal, stop searching 

astar_search([node(State,RevPath,G,_)|Rest],Goal,FinalRevPath,Cost):-
    findall(node(Next,[Next|RevPath],G1,F1),
        (connected(State,Next,StepCost),
         \+ member(Next,RevPath),
         G1 is G + StepCost,
         h(Goal,Next,H),
         F1 is G1 + H),
        Children),
    append(Rest, Children, OpenList),
    sort(4,@=<,OpenList, Sorted),
    astar_search(Sorted, Goal, FinalRevPath, Cost).