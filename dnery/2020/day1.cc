#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

// This is a linear (2n) search across two dimensions
int mult_of_pair_that_sums_to_2020(vector<int> values)
{
    sort(values.begin(), values.end());

    // Iterator starting from vector beginning
    vector<int>::iterator fwd = values.begin();

    // Iterator starting from vector ending
    vector<int>::reverse_iterator rev = values.rbegin();

    while (*fwd + *rev != 2020)
    {
        if (*fwd + *rev > 2020) { rev++; continue; }
        if (*fwd + *rev < 2020) { fwd++; continue; }
    }

    return *fwd * *rev;
}

// This is a binary search in one dimension with a linear (2n) search in two others
int mult_of_triple_that_sums_to_2020(vector<int> values)
{
    sort(values.begin(), values.end());

    // Binary search iterators
    vector<int>::iterator bin_beg = values.begin();
    vector<int>::iterator bin_end = values.end();
    vector<int>::iterator bin_mid = next(bin_beg, values.size() / 2);

    // Iterator starting from vector beginning
    vector<int>::iterator fwd = values.begin();
    vector<int>::iterator fwd_end = values.end();

    // Iterator starting from vector ending
    vector<int>::reverse_iterator rev = values.rbegin();
    vector<int>::reverse_iterator rev_end = values.rend();

    while (*bin_mid + *fwd + *rev != 2020)
    {
        // Reset the dudes
        fwd = values.begin();
        rev = values.rbegin();

        while (fwd != fwd_end && rev != rev_end)
        {
            if (*bin_mid + *fwd + *rev > 2020) { rev++; continue; }
            if (*bin_mid + *fwd + *rev < 2020) { fwd++; continue; }

            // This is the branch out; run time complexity is O(log(n) * 2n)
            return *bin_mid * *fwd * *rev;
        }

        if (fwd == fwd_end) { fwd--; } // Rewind to compare
        if (rev == rev_end) { rev--; } // Rewind to compare

        if (*bin_mid + *fwd + *rev < 2020)
        {
            bin_beg = bin_mid;
            bin_mid = next(bin_beg, distance(bin_beg, bin_end) / 2);
            continue;
        }

        if (*bin_mid + *fwd + *rev > 2020)
        {
            bin_end = bin_mid;
            bin_mid = next(bin_beg, distance(bin_beg, bin_end) / 2);
            continue;
        }
    }

    return *bin_mid * *fwd * *rev;
}

int main(int argc, char *argv[])
{
    int value;
    vector<int> values;
    while (cin >> value) { values.push_back(value); }

    // int ret = mult_of_pair_that_sums_to_2020(values); // part 1
    int ret = mult_of_triple_that_sums_to_2020(values); // part 2

    cout << ret << endl;

    return 0;
}


