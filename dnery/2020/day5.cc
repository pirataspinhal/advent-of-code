#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

int find_part_id(
        string::iterator    bsp_cur,
        size_t              partition_beg,  // Range begin is INCLUSIVE
        size_t              partition_end)  // Range end is EXCLUSIVE
{
    size_t dist = partition_end - partition_beg;
    if (dist == 1) { return partition_beg ; }
    size_t mid_offs = dist / 2;

    if (*bsp_cur == 'F' || *bsp_cur == 'L')
    { return find_part_id(++bsp_cur, partition_beg, partition_beg + mid_offs); }
    else
    { return find_part_id(++bsp_cur, partition_beg + mid_offs, partition_end); }
}

long calc_seat_id(string seat_bsp)
{
    string::iterator rows_beg = seat_bsp.begin();
    string::iterator cols_beg = next(rows_beg, 7);

    int row_id = find_part_id(rows_beg, 0, 128);
    int col_id = find_part_id(cols_beg, 0, 8);

    return long(row_id) * 8 + long(col_id);
}

long find_skipped_id(vector<long>& seat_ids)
{
    sort(seat_ids.begin(), seat_ids.end());

    for (auto seat_id = seat_ids.begin(); seat_id != prev(seat_ids.end()); ++seat_id)
    {
        if (*next(seat_id) - *seat_id == 2) { return *seat_id + 1; }
    }

    // We're bound to return earlier because of the input
    // This is just to keep the compiler happy
    return -1;
}

int main()
{
    string line;
    vector<string> seat_bsps;
    while (getline(cin, line)) { seat_bsps.push_back(line); }

    /*
     * Part 1
     *
    long max_seat_id = 0;
    for (auto& seat_bsp: seat_bsps)
    {
        long seat_id = calc_seat_id(seat_bsp);
        if (seat_id > max_seat_id) { max_seat_id = seat_id; }
    }
    cout << max_seat_id << endl;
     */

    /*
     * Part 2
     */
    vector<long> seat_ids;
    for (auto& seat_bsp: seat_bsps)
    {
        seat_ids.push_back(calc_seat_id(seat_bsp));
    }

    long my_seat = find_skipped_id(seat_ids);
    cout << my_seat << endl;

    return 0;
}
