-module(ex6_311328322).

-export([songList/1,songGen/3]).

songList(List) ->
    songList(List,[],digraph:new()).

songList([],Verticies,Graph) ->
    connect_graph(Verticies,Verticies,Graph), % connects all verticies to each other with the proper rules
    Num_of_edges = length(digraph:edges(Graph)),
    io:format("Number of edges: ~p\n", [Num_of_edges]),
    Graph;

songList([Song|T],Verticies,Graph) -> % add vertex to graph
    digraph:add_vertex(Graph, Song),
    songList(T,[Song|Verticies],Graph).

connect_graph([],_,Graph) ->
    Graph;
connect_graph([V|T],Graph_verticies,Graph) ->
    connect_vertex(V,Graph_verticies,Graph),
    connect_graph(T,Graph_verticies,Graph).


connect_vertex(_,[],Graph) ->
    Graph;
connect_vertex(V,[V|T],Graph) -> % do not connect to self , skip
    connect_vertex(V,T,Graph);
connect_vertex(V,[H|T],Graph) -> % connect to all other verticies with the proper rules
    Last_char = lists:last(V),
    First_char = hd(H),
    case First_char == Last_char of % if the last Char of the source equals the first Char of the destination
        true ->
            digraph:add_edge(Graph, V, H), % Edge from V to H
            connect_vertex(V,T,Graph);
        false ->
            connect_vertex(V,T,Graph) % dont connect
    end.

songGen(Graph,Start,End) -> 
    % given the graph i built, get_short_path gives us exactly what we need for songGen
    digraph:get_short_path(Graph,Start,End).