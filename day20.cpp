#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <sstream>
#include <set>
#include <utility>
#include <vector>

typedef std::pair<int, int> coord;
typedef std::pair<coord, coord> door; // Door from one coord to another

const std::map<char, coord> compass {
    {'E',   { 1,  0}},
    {'S',   { 0,  1}},
    {'W',   {-1,  0}},
    {'N',   { 0, -1}},
};

coord add_coords(coord a, coord b) {
    return {a.first + b.first, a.second + b.second};
}

size_t find_closing_bracket(std::string s, int i) {
    // Start searching from (foo (bar baz) (quz))
    //                here:  ^                  ^ to match this
    // Assumes brackets are balanced!
    int open = 1;
    while (open > 0) {
        open += (s.at(i) == '(');
        open -= (s.at(i) == ')');
        i++;
    }
    return i - 1;
}

size_t find_next_pipe(std::string s, int i) {
    int open = 1;
    while (open > 0) {
        open += (s.at(i) == '(');
        open -= (s.at(i) == ')');
        if (open == 1 && s.at(i) == '|')
            return i;
        i++;
    }
    return i  - 1;
}

class Maze {
    std::set<door> doors;
    std::string data;
    public:
    Maze(std::string input_data) {
        doors.clear();
        data = input_data;
        build_maze({{0, 0}});
    }

    coord add_path(coord start_location, std::string path) {
        // Adds all necessary doors in the given path. Returns end location.
        // The path should consist ONLY of N/S/E/W.
        coord previous = start_location;
        coord current;
        for (int i = 0; i < path.size(); i++) {
            current = add_coords(previous, compass.at(path.at(i)));
            doors.insert({previous, current});
            doors.insert({current, previous});
            previous = current;
        }
        return current;
    }

    std::set<coord> build_maze(std::set<coord> branch_tips) {
        return build_maze(branch_tips, data);
    }
    std::set<coord> build_maze(
        std::set<coord> branch_tips,
        std::string s
    ) {
        // Returns the end points of all "branches"
        int i = 0;
        while (i < s.size()) {
            //std::cout << s << std::endl << i << " " << s.at(i) << std::endl;
            switch (s.at(i)) {
            case '^':
                break;
            case '$':
                return branch_tips;
            case '(': {
                std::set<coord> new_tips;
                std::vector<std::string> substrings;
                i++;
                int j = i;
                while (s.at(j) != ')') {
                    j = find_next_pipe(s, j + 1);
                    substrings.push_back(s.substr(i, j - i));
                    //std::cout << "pushed substring " << s.substr(i, j - i) << std::endl;
                    i = j + 1;
                }
                for (std::string ss : substrings) {
                    for (coord nt : build_maze({branch_tips}, ss)) {
                        new_tips.insert(nt);
                    }
                }
                branch_tips = new_tips;
                //std::cout << "branch_tips.size() == " << branch_tips.size() << std::endl;
                i--; // to compensate for the i++ later
                break;
            }
            case ')':
            case '|':
                std::cout << "error: read '" << s.at(i) << "'" << std::endl;
                return branch_tips;
            case 'N':
            case 'S':
            case 'E':
            case 'W':
                std::set<coord> new_tips;
                for (coord bt : branch_tips) {
                    //std::cout << "("
                              //<< bt.first << "," << bt.second
                              //<< ") => ";
                    new_tips.insert(
                        add_path(bt, std::string(1, s.at(i)))
                    );
                    //std::cout << "("
                              //<< new_tips.back().first << "," << new_tips.back().second
                              //<< ")" << std::endl;
                }
                branch_tips = new_tips;
                break;
            }
            i++;
        }
        return branch_tips;
    }

    std::set<coord> get_adjacent(coord start) {
        std::set<coord> result;
        for (door d : doors) {
            if (d.first == start) {
                result.insert(d.second);
            }
            if (d.second == start) {
                result.insert(d.first);
            }
        }
        return result;
    }

    std::pair<int, int> find_greatest_distance(void) {
        int greatest_distance = 0;
        int n_distant_rooms = 0;
        std::set<coord> visited;
        std::queue<std::pair<coord, int>> to_explore;
        const coord start_location = {0, 0};
        to_explore.push({start_location, 0});
        while (to_explore.size() > 0) {
            std::pair<coord, int> node = to_explore.front();
            to_explore.pop();
            visited.insert(node.first);
            if (node.second > greatest_distance) {
                greatest_distance = node.second;
            }
            if (node.second >= 1000) {
                n_distant_rooms++;
            }
            for (coord xy : get_adjacent(node.first)) {
                if (!visited.count(xy)) {
                    to_explore.push({xy, node.second + 1});
                }
            }
        }
        return {greatest_distance, n_distant_rooms};
    }

    void display(void) {
        // TODO
        return;
    }
};

std::string slurp(std::string filename) {
    std::ifstream fp(filename);
    std::stringstream buffer;
    buffer << fp.rdbuf();
    fp.close();
    return buffer.str();
}

int main(void) {
    //std::cout << "Building map..." << std::endl;
    Maze m = Maze(slurp("input20.txt"));
    //m.display(); // Map's too big to do this (except to debug examples)
    //std::cout << "Finding greatest distance..." << std::endl;
    auto solution = m.find_greatest_distance();
    std::cout << solution.first << std::endl;
    std::cout << solution.second << std::endl;
    return 0;
}
