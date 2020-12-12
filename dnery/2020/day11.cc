#include <vector>
#include <string>
#include <sstream>
#include <iostream>

using namespace std;

char
rule_on_occupied(vector<string>& seatmap, vector<pair<int,int>> queries)
{
    int occupied_count = 0;
    for (auto& query: queries)
    {
        // Become empty if there are at least 4 (5 in part 2) occupied seats around it
        if (seatmap[query.first][query.second] == '#')
        {
            //if (occupied_count++ == 3) { return 'L'; } // part 1
            if (occupied_count++ == 4) { return 'L'; }   // part 2
        }
    }
    return '#';
}

char
rule_on_empty(vector<string>& seatmap, vector<pair<int,int>> queries)
{
    for (auto& query: queries)
    {
        // Become occupied only if there are no occupied seats around it
        if (seatmap[query.first][query.second] == '#')
        {
            return 'L';
        }
    }
    return '#';
}

// query mode: part 1
vector<pair<int,int>>
make_queries1(vector<string>& seatmap, int row, int col)
{
    vector<pair<int,int>> ret;
    for (int i = -1; i <= 1; ++i)
    {
        for (int j = -1; j <= 1; ++j)
        {
            // Ignore the current seat itself
            if (row + i == row && col + j == col) { continue; }

            // Include only offsets within limits
            if (row + i >= 0 && row + i < seatmap.size() &&
                col + j >= 0 && col + j < seatmap[0].size())
            {
                ret.push_back({row + i, col + j});
            }
        }
    }
    return ret;
}

// query mode: part 2
vector<pair<int,int>>
make_queries2(vector<string>& seatmap, int row, int col)
{
    vector<pair<int,int>> ret;
    for (int i = -1; i <= 1; ++i)
    {
        for (int j = -1; j <= 1; ++j)
        {
            // Ignore the current seat itself
            if (row + i == row && col + j == col) { continue; }

            // Include only offsets within limits
            int cursor = 1;
            int rayi = row + (i * cursor);
            int rayj = col + (j * cursor);
            while (rayi >= 0 && rayi < seatmap.size() &&
                   rayj >= 0 && rayj < seatmap[0].size())
            {
                if (seatmap[rayi][rayj] != '.')
                {
                    ret.push_back({rayi, rayj});
                    break;
                }
                cursor++;
                rayi = row + (i * cursor);
                rayj = col + (j * cursor);
            }
        }
    }
    return ret;
}

int
apply_rules(vector<string>& seatmap)
{
    int total_changes = 0;
    vector<pair<pair<int, int>, char>> patches;

    // Create patches
    for (int irow = 0; irow < seatmap.size(); ++irow)
    {
        for (int iseat = 0; iseat < seatmap[irow].size(); ++iseat)
        {
            char ans = seatmap[irow][iseat];
            if (seatmap[irow][iseat] == 'L')
            {
                //ans = rule_on_empty(seatmap, make_queries1(seatmap, irow, iseat));    // part 1
                ans = rule_on_empty(seatmap, make_queries2(seatmap, irow, iseat));      // part 2
            }
            else if (seatmap[irow][iseat] == '#')
            {
                //ans = rule_on_occupied(seatmap, make_queries1(seatmap, irow, iseat)); // part 1
                ans = rule_on_occupied(seatmap, make_queries2(seatmap, irow, iseat));   // part 2
            }

            if (ans != seatmap[irow][iseat])
            {
                patches.push_back({{irow, iseat}, ans});
                total_changes++;
            }
        }
    }

    // Apply patches
    for (auto& patch: patches)
    {
        int i = patch.first.first;
        int j = patch.first.second;
        seatmap[i][j] = patch.second;
    }

    return total_changes;
}

int
count(vector<string>& seatmap)
{
    int occupied_seats = 0;
    for (auto& row: seatmap)
    {
        for (auto& seat: row)
        {
            if (seat == '#') { occupied_seats++; }
        }
    }
    return occupied_seats;
}

int main()
{
    string line;
    vector<string> seatmap;

    while (getline(cin, line))
    {
        seatmap.push_back(line);
    }

    while (apply_rules(seatmap));

    for (auto& row: seatmap)
    {
        cout << row << endl;
    }

    cout << count(seatmap) << endl;

    return 0;
}
