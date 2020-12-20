#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

vector<pair<int,int>>
parse_buses(string& unparsed_buses)
{
    vector<pair<int,int>> buses;
    size_t iter = 0;
    size_t beg = 0;
    size_t cur = 0;

    do {
        // Extract substring as int
        cur = unparsed_buses.find(',', beg);
        string parcel = unparsed_buses.substr(beg, cur - beg);
        if (parcel.compare("x") != 0) { buses.push_back({stoi(parcel), iter}); }
        // The second element in the push above is the mod result in the chinese remainder

        // Move offset beginning cursor
        beg = cur + 1;
        iter++;
    } while (cur < unparsed_buses.size());

    return buses;
}

// part 1
pair<int,int>
best_bus_id_and_wait_time(int timestamp, vector<int>& buses)
{

    vector<pair<int,int>> wait_times_and_buses;

    for (auto& bus: buses)
    {
        int time_diff = timestamp % bus;
        // Optimal bus requires 0 wait time
        if (time_diff == 0) { return {bus, 0}; }

        wait_times_and_buses.push_back({bus - time_diff, bus});
    }

    // Sort the pairs by the first element (wait time) and return the lowest pair
    sort(wait_times_and_buses.begin(), wait_times_and_buses.end());
    return wait_times_and_buses.front();
}

// part 2
long long
sieve_chinese_remainder(vector<pair<int,int>>& buses)
{
    // Arithmetic progression term holding the final result
    long long overall_sum = 0;

    // Carries the cumulative product of the dividends (n1*n2*n3...), ni > 1
    long long dividend_carry = 1;

    // x === a_1 (mod n_1)
    // x === a_2 (mod n_2)
    // x === a_3 (mod n_3)
    // ...
    // x === a_k (mod n_k)
    //
    // -> n_1 * n_2 * ... * n_k = N, 0 <= x < N
    // -> n_i are all pairwise coprimes and n_i > 1
    // -> ensure that 0 <= a_i < n_i (hence the % below)
    // -> below, a_i is bus->second and n_i is bus->first

    for (auto bus = buses.begin(); bus != prev(buses.end()); ++bus)
    {
        // What we're seeking for the current equation in the congruence system
        long long mod_result = next(bus)->first - (next(bus)->second % next(bus)->first);

        // Something something dark side something something complete
        dividend_carry *= bus->first;

        // Increase number until congruence is satisfied
        while (overall_sum % next(bus)->first != mod_result)
        {
            overall_sum += dividend_carry;
        }
    }

    return overall_sum;
}

int main()
{
    int timestamp;
    cin >> timestamp;
    cin.ignore(INT_MAX, '\n');

    string unparsed_buses;
    getline(cin, unparsed_buses);
    // getline already clears cin up too '\n'
    auto buses = parse_buses(unparsed_buses);



    // DEBUG TODO REMOVE
    for (auto& bus: buses)
    {
        printf("x === %3d or %3d (mod %3d)\n", -bus.second, bus.first - (bus.second % bus.first), bus.first);
    }

    auto best_timestamp = sieve_chinese_remainder(buses);
    //auto best_pair = best_bus_id_and_wait_time(timestamp, buses);     // part 1
    cout << "x = " << best_timestamp << endl;                           // part 2

    return 0;
}
