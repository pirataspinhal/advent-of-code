#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

vector<int>::iterator next_pos_h(
        vector<int>::iterator beg,
        vector<int>::iterator pos,
        vector<int>::iterator end,
        int hoffset)
{
    int new_pos = (distance(beg, pos) + hoffset) % distance(beg, end);
    return new_pos > 0 ? next(beg, new_pos) : beg;
}

vector<vector<int>>::iterator next_pos_v(
        vector<vector<int>>::iterator beg,
        vector<vector<int>>::iterator pos,
        vector<vector<int>>::iterator end,
        int voffset)
{
    int new_pos = min(distance(beg, pos) + voffset, distance(beg, end));
    return next(beg, new_pos);
}


int count_them_trees(vector<vector<int>> data, int hoffset, int voffset)
{
    int trees_found = 0;

    vector<vector<int>>::iterator vbeg = data.begin();
    vector<vector<int>>::iterator vend = data.end();
    vector<vector<int>>::iterator vpos = vbeg;

    vector<int>::iterator hbeg = data[0].begin();
    vector<int>::iterator hend = data[0].end();
    vector<int>::iterator hpos = hbeg;

    int data_breadth = distance(hbeg, hend);

    while ((vpos = next_pos_v(vbeg, vpos, vend, voffset)) != vend)
    {
        // Move iterators to this line
        auto new_hbeg = vpos->begin();
        hpos = next(new_hbeg, distance(hbeg, hpos));
        hend = next(new_hbeg, data_breadth);
        hbeg = new_hbeg;

        // Recalculate horizontal offset
        hpos = next_pos_h(hbeg, hpos, hend, hoffset);
        if (*hpos == '#') { trees_found++; } // Count tree :)
    }

    return trees_found;
}

int main()
{
    string line;
    vector<vector<int>> data;
    vector<pair<int, int>> slopes{
        {1, 1},
        {3, 1},
        {5, 1},
        {7, 1},
        {1, 2}};

    while(getline(cin, line))
    {
        // Build the input array
        data.push_back(vector<int>());
        for (auto& it: line) { data.back().push_back(it); }
    }

    // int ntrees = count_them_trees(data, 3, 1);   // part 1

    size_t ntrees = 1;                                                                      // part 2
    for (auto& slopeconfig: slopes)                                                         // part 2
    {                                                                                       // part 2
        ntrees *= size_t(count_them_trees(data, slopeconfig.first, slopeconfig.second));    // part 2
    }                                                                                       // part 2

    cout << ntrees << endl;

    return 0;
}


