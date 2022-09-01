#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

class Particle {
    std::pair<int, int> position;
    std::pair<int, int> velocity;
    public:
    Particle(std::string line) { // constructor
        // position is 10-15 and 18-23 (both length 6)
        // velocity is 36-37 and 40-41 (both length 2)
        position.first  = std::stoi(line.substr(10, 6));
        position.second = std::stoi(line.substr(18, 6));
        velocity.first  = std::stoi(line.substr(36, 2));
        velocity.second = std::stoi(line.substr(40, 2));
    }
    void step(void) {
        position = {
            position.first + velocity.first,
            position.second + velocity.second
        };
        return;
    }
    int getx(void) {
        return position.first;
    }
    int gety(void) {
        return position.second;
    }
};

std::pair<int, int> get_width_height(std::vector<Particle> ps) {
    std::vector<int> xs;
    std::vector<int> ys;
    for (size_t i = 0; i < ps.size(); i++) {
        xs.push_back(ps.at(i).getx());
        ys.push_back(ps.at(i).gety());
    }
    return {
        *std::max_element(xs.begin(), xs.end())
            - *std::min_element(xs.begin(), xs.end()),
        *std::max_element(ys.begin(), ys.end())
            - *std::min_element(ys.begin(), ys.end())
    };
}

void print_particles(std::vector<Particle> ps) {
    std::vector<int> xs;
    std::vector<int> ys;
    for (size_t i = 0; i < ps.size(); i++) {
        xs.push_back(ps.at(i).getx());
        ys.push_back(ps.at(i).gety());
    }
    int minx = *std::min_element(xs.begin(), xs.end());
    int maxx = *std::max_element(xs.begin(), xs.end());
    int miny = *std::min_element(ys.begin(), ys.end());
    int maxy = *std::max_element(ys.begin(), ys.end());

    for (int y = miny; y <= maxy; y++) {
        for (int x = minx; x <= maxx; x++) {
            std::string symbol = " ";
            for (size_t i = 0; i < ps.size(); i++) {
                Particle p = ps.at(i);
                if (p.getx() == x && p.gety() == y) {
                    symbol = "@";
                }
            }
            std::cout << symbol;
        }
        std::cout << std::endl;
    }
    return;
}

int main(void) {
    std::ifstream inputfile("input10.txt");
    std::string line;
    std::vector<Particle> particles;
    while (std::getline(inputfile, line)) {
        particles.push_back(Particle(line));
    }
    inputfile.close();
    std::pair<int, int> dims = get_width_height(particles);
    int timer = 0;
    while (dims.second > 10) { // trial and error
        for (size_t i = 0; i < particles.size(); i++) {
            particles.at(i).step();
        }
        dims = get_width_height(particles);
        timer++;
    }
    print_particles(particles);
    std::cout << timer << std::endl;
    return 0;
}
