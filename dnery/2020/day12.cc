#include <algorithm>
#include <iostream>
#include <vector>
#include <string>

using namespace std;

struct Boat
{
    enum Direction {
        F = (int)'F',
        L = (int)'L',
        R = (int)'R',
        N = (int)'N',
        E = (int)'E',
        S = (int)'S',
        W = (int)'W'
    };

    Direction curr_dir;
    long long curr_e;
    long long curr_n;

    long long waypoint_e;
    long long waypoint_n;

    Boat() :
        curr_dir(Direction::E), curr_e(0), curr_n(0), waypoint_e(10), waypoint_n(1)
    {}

    void move(int units, Direction dir);
    void turn(int angle, Direction dir);

    void move_waypoint_or_towards_it(int units, Direction dir);
    void turn_waypoint_around_boat(int angle, Direction dir);

    long long m_distance() { return abs(curr_e) + abs(curr_n); }
};

void // part 1
Boat::move(int units, Direction dir)
{
    // If the move is a turn, delegate to 'turn' function
    if (dir == Direction::L || dir == Direction::R) { turn(units, dir); return; }

    // Bit of a shortcut
    Direction go_towards = dir;
    if (go_towards == Direction::F) { go_towards = curr_dir; }

    switch (go_towards)
    {
        case Direction::N:
            curr_n += units;
            break;
        case Direction::E:
            curr_e += units;
            break;
        case Direction::S:
            curr_n -= units;
            break;
        case Direction::W:
            curr_e -= units;
            break;
        default: break;
    }
}

void // part 1
Boat::turn(int angle, Direction dir)
{
    vector<Direction> lookup{
        Direction::N,
        Direction::E,
        Direction::S,
        Direction::W
    };

    // Turn left turn into complimentary right turn
    if (dir == Direction::L)
    {
        angle = 360 - angle;
    }
    int dir_hops = angle / 90;

    // Find current direction in direction vector
    auto curr = lookup.begin();
    while (*curr != curr_dir) { curr++; }

    // Turning == hopping in direction vector
    int final_pos = (distance(lookup.begin(), curr) + dir_hops) % lookup.size();
    curr = next(lookup.begin(), final_pos);

    // Finally set my direction
    curr_dir = *curr;
}

void // part 2
Boat::move_waypoint_or_towards_it(int units, Direction dir)
{
    // If the move is a turn, delegate to 'turn' function
    if (dir == Direction::L || dir == Direction::R) { turn_waypoint_around_boat(units, dir); return; }

    // Bit of a shortcut
    if (dir == Direction::F)
    {
        curr_n += units * waypoint_n;
        curr_e += units * waypoint_e;
        return;
    }

    switch (dir)
    {
        case Direction::N:
            waypoint_n += units;
            break;
        case Direction::E:
            waypoint_e += units;
            break;
        case Direction::S:
            waypoint_n -= units;
            break;
        case Direction::W:
            waypoint_e -= units;
            break;
        default: break;
    }
}

void // part 2
Boat::turn_waypoint_around_boat(int angle, Direction dir)
{
    // Turn right turn into complimentary left turn
    if (dir == Direction::R) { angle = 360 - angle; }

    int turns = angle / 90;

    while (turns--)
    {
        /*
         * Suppose waypoint_(e,n) form a vertical vector:
         *
         * | waypoint_e |
         * | waypoint_n |
         *
         * The operation below is the same as multiplying
         * our vector by a 2d rotation matrix, with
         * theta being 90 degrees:
         *
         * Rot * | waypoint_e | = | waypoint_e' |
         *       | waypoint_n |   | waypoint_n' |
         *
         * Where:
         *
         * Rot = | cos(90)  -sin(90) | = | 0  -1 |
         *       | sin(90)   cos(90) |   | 1   0 |
         *
         * So:
         *
         * Rot * | waypoint_e | = | -waypoint_n |
         *       | waypoint_n |   |  waypoint_e |
         */
        int swap   =  waypoint_e;
        waypoint_e = -waypoint_n;
        waypoint_n =  swap;
    }
}

pair<Boat::Direction, int>
parse_cmd(string cmd)
{
    auto dir = static_cast<Boat::Direction>(cmd[0]);
    auto units_section = cmd.substr(1, distance(next(cmd.begin()), cmd.end()));
    auto units = stoi(units_section);
    return {dir, units};
}


int main()
{
    Boat boat{};
    string line;

    while (cin >> line)
    {
        auto cmd = parse_cmd(line);
        //boat.move(cmd.second, cmd.first);                      // part 1
        boat.move_waypoint_or_towards_it(cmd.second, cmd.first); // part 2
    }

    cout << "Manhattan distance: " << boat.m_distance() << endl;

    return 0;
}
