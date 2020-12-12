#include <algorithm>
#include <iostream>
#include <climits>
#include <stdlib.h>
#include <vector>
#include <string>
#include <unordered_map>

using namespace std;

#define WINSIZE 25

int
insert_checked(vector<long long>& nums, long long num)
{
    unordered_map<long long, char> sums;

    // Build the sum map for the past WINSIZE elements
    for (auto it = prev(nums.end(), WINSIZE); it != prev(nums.end()); ++it)
    {
        for (auto jt = next(it); jt != nums.end(); ++jt)
        {
            sums[*it + *jt] = 1;
        }
    }

    if (!sums[num]) { return 1; }
    nums.push_back(num);
    return 0;
}

pair<vector<long long>::iterator,vector<long long>::iterator>
find_sum_window(vector<long long>& nums, long long num)
{
    long long current_sum = 0;
    auto window_beg = nums.begin();
    auto window_end = nums.begin();

    // Do a flexible window
    while (current_sum != num)
    {
        // Grow if sum is too small
        if (current_sum + *window_end <= num)
        {
            current_sum += *window_end;
            ++window_end;
            continue;
        }

        // Shrink if sum is too large
        if (current_sum + *window_end >= num)
        {
            current_sum -= *window_beg;
            ++window_beg;
            continue;
        }
    }

    return {window_beg, window_end};
}

long long
sum_extremes(vector<long long>::iterator beg, vector<long long>::iterator end)
{
    long long lo = LLONG_MAX;
    long long hi = LLONG_MIN;

    for (auto it = beg; it != end; ++it)
    {
        if (*it > hi) { hi = *it; }
        if (*it < lo) { lo = *it; }
    }

    return lo + hi;
}

int main()
{
    long long num;
    vector<long long> nums;

    while (cin >> num)
    {
        if (nums.size() < WINSIZE)
        {
            nums.push_back(num);
            continue;
        }

        if (insert_checked(nums, num))
        {
            // part 1
            cout << num << endl;

            // part 2
            auto window = find_sum_window(nums, num);
            cout << sum_extremes(window.first, window.second) << endl;

            // we done
            break;
        }
    }

    return 0;
}
