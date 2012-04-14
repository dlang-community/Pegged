/**
A cheap way to imitate AA at compile-time.
*/
module pegged.utils.associative;

import std.array;
import std.conv;
import std.stdio;
import std.traits;
import std.typecons;

bool lessThan(T)(T a, T b)
{
    return a < b;
}

version(none)
{
enum AsSet { no, yes }

struct Sorted(T, alias pred = lessThan, AsSet as = AsSet.yes)
{
    T[] values;
    
    this(T value)
    {
        values = [value];
    }
    
    this(T[] values)
    {
        foreach(v; values) insert(v);
    }
    
    int findPos(T value, T[] values)
    {
        if (values.length == 0)
        {
            return 0;
        }
        else if (pred(value, values[0])) // value < min value
        {
            return 0;
        }
        else if (pred(values[$-1], value)) // value > max value
        {
            return values.length;
        }
        else if (values.length == 1 && !pred(value, values[0]) && !pred(values[0], value))
            return 0;
        else if (pred(value, values[$/2]))
            return findPos(value, values[0..$/2]);
        else
            return values.length/2 + findPos(value, values[$/2..$]);
    }
    
    void insert(T value)
    {
        int pos = findPos(value, values);
        
        static if (as == AsSet.yes)
            if (pos < values.length && !pred(value, values[pos]) && !pred(values[pos],value)) // !(a < b) && !(b > a) => a==b
                return; // already there, no need to insert.

        values = values[0..pos] ~ value ~ values[pos..$];
    }
    
    void insert(T[] values)
    {
        foreach(v; values) insert(v);
    }
    
    void insert(Sorted s)
    {
        foreach(v; s.values) insert(v);
    }
    
    Sorted opBinary(string op, U)(U u) if (op == "~")
    {
        auto s = this;
        s.insert(u);
        return s;
    }
    
    void opOpAssign(string op, U)(U u) if (op == "~")
    {
        insert(u);
    }
    
    alias insert push;
    
    void pop()
    {
        values.length -= 1;
    }
    
    void flush()
    {
        values.length = 0;
    }
    
    string toString() @property
    {
        return to!string(values);
    }
    
    bool empty() @property
    {
        return values.empty;
    }
    
    void popFront() @property
    {
        values.popFront;
    }
    
    T front() @property
    {
        return values.front;
    }
    
    size_t length() @property
    {
        return values.length;
    }
    
    Sorted save() @property
    {
        return this;
    }
    
    T opIndex(size_t i)
    {
        return values[i];
    }
    
    Sorted opSlice(size_t i, size_t j)
    {
        auto s = this;
        s.values = s.values[i..j];
        return s;
    }
}

Sorted!(T, pred, as) sorted(T, alias pred = lessThan, AsSet as = AsSet.yes)(T value) if (!isArray!T)
{
    return Sorted!(T, pred, as)(value);
}

Sorted!(T, pred, as) sorted(T, alias pred = lessThan, AsSet as = AsSet.yes)(T[] value)
{
    return Sorted!(T, pred, as)(value);
}
}

struct AssociativeList(Key, Value, alias pred = lessThan)
{
    alias Tuple!(Key,Value) KV;
    KV[] pairs;
    
    this(KV pair)
    {
        pairs = [pair];
    }
    
    this(KV[] pairs)
    {
        foreach(p; pairs) insert(p);
    }
    
    this(Key key, Value value)
    {
        insert(tuple(key,value));
    }

    size_t findPos(KV pair, KV[] pairs)
    {
        if (pairs.length == 0)
        {
            return 0;
        }
        else if (pred(pair[0], pairs[0][0])) // pair < min pair
        {
            return 0;
        }
        else if (pred(pairs[$-1][0], pair[0])) // pair > max pair
        {
            return pairs.length;
        }
        else if (pairs.length == 1 && !pred(pair[0], pairs[0][0]) && !pred(pairs[0][0], pair[0]))
            return 0;
        else if (pred(pair[0], pairs[$/2][0]))
            return findPos(pair, pairs[0..$/2]);
        else
            return pairs.length/2 + findPos(pair, pairs[$/2..$]);
    }
    
    void insert(KV pair)
    {
        auto pos = findPos(pair, pairs);
        if (pos < pairs.length && !pred(pair[0], pairs[pos][0]) && !pred(pairs[pos][0],pair[0])) // !(a < b) && !(b > a) => a==b
            pairs[pos][1] = pair[1];
        else
            pairs = pairs[0..pos] ~ pair ~ pairs[pos..$];
    }
    
    void insert(KV[] pairs)
    {
        foreach(p; pairs) insert(p);
    }
    
    void insert(AssociativeList al)
    {
        foreach(pair; al.pairs) insert(pair);
    }
    
    void insert(Key key, Value value)
    {
        insert(tuple(key,value));
    }
    
    AssociativeList opBinary(string op, U)(U u) if (op == "~")
    {
        auto s = this;
        s.insert(u);
        return s;
    }
    
    void opOpAssign(string op, U)(U u) if (op == "~")
    {
        insert(u);
    }
    string toString() @property
    {
        string result = "[";
        foreach(kv; pairs) result ~= to!string(kv[0]) ~ ":" ~ to!string(kv[1]) ~ ", ";
        if (result.length > 1) result.length -= 2; // get rid of the last ", "
        return result ~ "]";
    }
    
    void flush()
    {
        pairs.length = 0;
    }
    
    void discard(Key key)
    {
        auto pos = findWhere(key, pairs);
        if (pos == -1)
            throw new Exception("No value with key " ~ to!string(key));
        else
            pairs = pairs[0..pos] ~ pairs[pos+1..$];
    }
    
    bool empty() @property
    {
        return pairs.empty;
    }
    
    void popFront() @property
    {
        pairs.popFront();
    }
    
    KV front() @property
    {
        return pairs.front();
    }
    
    size_t length() @property
    {
        return pairs.length;
    }
    
    AssociativeList save() @property
    {
        return this;
    }
    
    size_t findWhere(Key key, KV[] pairs)
    {
        if (pairs.length == 0)
        {
            return -1;
        }
        else if (pred(key, pairs[0][0])) // value < min value
        {
            return 0;
        }
        else if (pred(pairs[pairs.length-1][0], key)) // value > max value
        {
            return -1;
        }
        else if (pairs.length == 1 && !pred(key, pairs[0][0]) && !pred(pairs[0][0], key))
            return 0;
        else if (pred(key, pairs[pairs.length/2][0]))
            return findWhere(key, pairs[0..pairs.length/2]);
        else
        {
            auto pos2 = findWhere(key, pairs[pairs.length/2..pairs.length]);
            return (pos2 == -1) ? -1 : pairs.length/2 + pos2;
        }
    }

    Value opIndex(Key key)
    {
        auto pos = findWhere(key, pairs);
        if (pos == -1)
            throw new Exception("No value with key " ~ to!string(key));
        else
            return pairs[pos][1];
    }
    
    void opIndexAssign(Value value, Key key)
    {
        insert(tuple(key,value));
    }
    
    bool opIn_r(Key key)
    {
        return (findWhere(key,pairs) != -1);
    }
}
