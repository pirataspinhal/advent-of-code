#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

// part 1
pair<int, int>
find_rating_jumps_of1_and3(vector<int>& adapters)
{
    int jumpsof1 = 0;
    int jumpsof3 = 0;

    for (auto i = adapters.begin(); i != prev(adapters.end()); ++i)
    {
        for (auto j = next(i); j != adapters.end() && j <= next(i, 3); ++j)
        {
            if (*j - *i == 1) { jumpsof1++; break; }
            if (*j - *i == 3) { jumpsof3++; break; }
        }
    }

    return {jumpsof1, jumpsof3};
}

// part 2
vector<int>
valid_jumps_for_pos(vector<int>& adapters, int pos)
{
    vector<int> valid_jumps;

    // For each element between pos - 1 and pos - 3 inclusive
    for (int ri = pos - 1; ri >= 0 && ri >= pos - 3; --ri)
    {
        int jump_diff = adapters[pos] - adapters[ri];
        if (jump_diff >= 1 && jump_diff <= 3)
        {
            valid_jumps.push_back(ri);
        }
    }

    return valid_jumps;
}

// part 2
long long
count_arrangements(vector<int>& adapters, vector<long long>& memo, int pos)
{
    // If position is memo'd, just return it
    if (memo[pos] != -1) { return memo[pos]; }

    long long nchoices = 0;
    // Sum choices for all possible branches from here
    for (auto& choice: valid_jumps_for_pos(adapters,pos))
    {
        nchoices += count_arrangements(adapters, memo, choice);
    }

    // If there are no choices, then this is a recursion end
    memo[pos] = max(nchoices, static_cast<long long>(1));

    return memo[pos];
}

int main()
{
    int rating;
    vector<int> adapters;

    while (cin >> rating)
    {
        adapters.push_back(rating);
    }

    // Setup input
    adapters.push_back(0);
    sort(adapters.begin(), adapters.end());
    adapters.push_back(adapters.back() + 3);

    // Do the stuff

#if 0
    // part 1
    auto jumps = find_rating_jumps_of1_and3(adapters);
    cout << jumps.first * jumps.second << endl;
#endif

    // part 2
    vector<long long> memo(adapters.size(), -1);
    cout << count_arrangements(adapters, memo, adapters.size() - 1) << endl;

    return 0;
}
