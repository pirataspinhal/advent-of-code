#include <iostream>
#include <string>
#include <cctype>
#include <map>

using namespace std;


bool is_ws_only(string& line)
{
    for (auto& ch: line)
    {
        // Check if string is white space only
        if (!isspace(ch)) { return false; }
    }
    return true;
}

size_t check_ans_pt1(map<char,int>& yes)
{
    size_t ret = 0;
    for (auto& it: yes)
    {
        // Count questions to which _anyone_ answered yes
        ret++;
    }
    return ret;
}


size_t check_ans_pt2(map<char,int>& yes)
{
    size_t ret = 0;
    for (auto& it: yes)
    {
        // Count questions to which _everyone_ answered yes
        if (it.first != '_' && it.second == yes['_'])
        {
            ret++;
        }
    }
    return ret;
}

void debug_map(map<char,int>& yes)
{
    for (auto& it: yes)
    {
        cout << it.first << " " << it.second << endl;
    }
    cout << "size: " << yes.size() << endl << endl;
}

int main()
{
    string line;
    map<char,int> yes;
    size_t yes_count = 0;

    while (getline(cin, line))
    {
        if (is_ws_only(line))
        {
            // debug_map(yes);
            // yes_count += check_ans_pt1(yes); // part 1
            yes_count += check_ans_pt2(yes);    // part 2
            yes.clear();
            continue;
        }

        for (auto& ch: line)
        {
            // if (!isspace(ch)) { yes[ch] = 1; } // part 1
            if (!isspace(ch)) { yes[ch]++; }      // part 2
        }
        yes['_']++; // store the number of participants in '_' (for part 2)
    }

    cout << yes_count << endl;

    return 0;
}
