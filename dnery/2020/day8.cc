#include <iostream>
#include <string>
#include <vector>
#include <ctime>
#include <map>

using namespace std;

struct ProgramState
{
    long long acc;
    long long cnt;
    char finished;

    ProgramState() :
        acc(0), cnt(0), finished(0)
    {}
};

pair<string,long long>
parse_line(string line)
{
    // command: [0..3)
    int cmd_offs = 0;
    int cmd_size = 3;

    // signal:  [4..5)
    int sig_offs = 4;
    int sig_size = 1;

    // value:   [5..end)
    int val_offs = 5;
    int val_size = line.size() - val_offs;

    // parse stuff
    string cmd    = line.substr(cmd_offs, cmd_size);
    string sig    = line.substr(sig_offs, sig_size);
    long long val = stoll(line.substr(val_offs, val_size));

    // Adjust signal
    if (sig.compare("-") == 0) { val = -val; }

    return { cmd, val };
}

void
cmd_acc(pair<string,long long> tokens, ProgramState& state)
{
    state.acc += tokens.second;
    state.cnt++;
}

void
cmd_jmp(pair<string,long long> tokens, ProgramState& state)
{
    state.cnt += tokens.second;
}

void
cmd_nop(pair<string,long long> tokens, ProgramState& state)
{
    state.cnt++;
}

void
run_until_repeat(vector<string>& program, ProgramState& state)
{
    map<long long, char> visited;   // Marks visited program counters
    map<string, void (*)(pair<string,long long>,ProgramState&)> jmp_to_cmd =
    {
        {"acc", cmd_acc},
        {"jmp", cmd_jmp},
        {"nop", cmd_nop}
    };

    auto it = program.begin();
    while (it != program.end())
    {
        if (visited[state.cnt]) { return; }

        // Visited!
        visited[state.cnt] = 1;

        // Parse program line
        auto tokens = parse_line(*it);

        // Execute appropriate command
        jmp_to_cmd[tokens.first](tokens, state);

        // Jump to appointed program counter
        it = next(program.begin(), state.cnt);
    }

    state.finished = 1;
}

void // part 2
patch_program_rng(vector<string>& program)
{
    int cmd_offs = 0;
    int cmd_size = 3;
    vector<long long> positions;

    // Find all "nop" and "jmp" positions
    for (long long cnt = 0; cnt < program.size(); ++cnt)
    {
        if (program[cnt].compare(cmd_offs, cmd_size, "jmp") == 0 ||
            program[cnt].compare(cmd_offs, cmd_size, "nop") == 0)
        {
            positions.push_back(cnt);
        }
    }

    // Reset the seed for upcoming "rand()" calls
    srand((unsigned)time(NULL));

    // Try to monkey patch each "nop" and "jmp" command randomly
    do {
        auto it = next(positions.begin(), rand() % positions.size());

        // Modify the command
        string old = program[*it].substr(cmd_offs, cmd_size);
        if (program[*it].compare(cmd_offs, cmd_size, "jmp") == 0) { program[*it].replace(cmd_offs, cmd_size, "nop"); }
        else                                                      { program[*it].replace(cmd_offs, cmd_size, "jmp"); }

        // Check if program runs to completion
        ProgramState state{};
        run_until_repeat(program, state);
        if (state.finished) { return; }

        // Restore old command
        program[*it].replace(cmd_offs, cmd_size, old);

        // Clear position
        positions.erase(it);
    } while (positions.size());

    cout << "Monkey patching failed!" << endl;
}


int main()
{
    string line;
    ProgramState state{};
    vector<string> program;

    // Read input program
    while (getline(cin, line)) { program.push_back(line); }

    // Run until program counter is repeated
    patch_program_rng(program); // part 2
    run_until_repeat(program, state);
    cout << state.acc << endl;

    return 0;
}
