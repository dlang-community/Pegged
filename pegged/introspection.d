module pegged.introspection;

import pegged.grammar;

bool[string][string] callGraph(string s)
{
    bool[string] findIdentifiers(ParseTree p)
    {
        bool[string] idList;
        if (p.name == "Identifier")
            idList[p.matches[0]] = true;
        else
            foreach(child; p.children)
                foreach(name; findIdentifiers(child).keys)
                    idList[name] = true;

        return idList;
    }

    bool[string][string] graph;
    ParseTree p = Pegged(s).children[0];
    foreach(definition; p.children)
        if (definition.name == "Definition")
        {
            auto ids = findIdentifiers(definition.children[2]);
            graph[definition.matches[0]] = ids;
            foreach(id, _; ids) // getting possible external calls
                if (id !in graph)
                    graph[id] = (bool[string]).init;
        }

    return graph;
}

enum Recursive { no, direct, indirect }

/// Transitive closure of a call graph
bool[string][string] closure(bool[string][string] graph)
{
    bool[string][string] path;
    foreach(rule, children; graph) // deep-dupping, to avoid children aliasing
        path[rule] = children.dup;
    
    bool changed = true;
    
    while(changed)
    {
        changed = false;
        foreach(rule1; graph.keys)
            foreach(rule2; graph.keys)
                if (rule2 in path[rule1])
                    foreach(rule3; graph.keys)
                        if (rule3 in path[rule2] && rule3 !in path[rule1])
                        {
                            path[rule1][rule3] = true;
                            changed = true;
                        }
    }
    
    return path;
}


Recursive[string] recursions(bool[string][string] graph)
{
    bool[string][string] path = closure(graph);
    
    Recursive[string] result;
    foreach(rule, children; path)
    {
        result[rule] = Recursive.no;
        if (rule in children)
        {
            if (rule in graph[rule])
                result[rule] = Recursive.direct;
            else
                result[rule] = Recursive.indirect;
        }
    }
                
    return result;
}
