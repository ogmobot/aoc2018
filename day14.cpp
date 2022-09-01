#include <iostream>
#include <vector>

std::vector<int> to_digits(size_t n);

class RecipeList {
    int index;
    int jndex;
    std::vector<int> recipes;
    void step(void) {
        int sum = recipes.at(index) + recipes.at(jndex);
        if (sum > 9) {
            recipes.push_back(1);
        }
        recipes.push_back(sum % 10);
        index = (index + recipes.at(index) + 1) % recipes.size();
        jndex = (jndex + recipes.at(jndex) + 1) % recipes.size();
    }
    public:
    RecipeList(void) {
        recipes.clear();
        recipes.push_back(3);
        recipes.push_back(7);
        index = 0;
        jndex = 1;
    }
    void print(void) {
        for (size_t i = 0; i < recipes.size(); i++) {
            std::cout << recipes.at(i) << " ";
        }
        std::cout << std::endl;
        return;
    }
    void solve_1(size_t n) {
        while (recipes.size() < n + 10)
            this->step();
        for (size_t i = 0; i < 10; i++) {
            std::cout << recipes.at(n + i);
        }
        std::cout << std::endl;
        return;
    }
    void solve_2(size_t n) {
        std::vector<int> digits = to_digits(n);
        size_t starting_index = 0;
        while (true) {
            while (starting_index + digits.size() >= recipes.size()) {
                this->step();
            }
            bool possible = true;
            for (size_t i = 0; i < digits.size(); i++) {
                if (digits.at(i) != recipes.at(starting_index + i)) {
                    possible = false;
                    break;
                }
            }
            if (possible)
                break;
            starting_index++;
        }
        std::cout << starting_index << std::endl;
        return;
    }
};

std::vector<int> to_digits(size_t n) {
    std::vector<int> v;
    v.clear();
    while (n > 0) {
        v.insert(v.begin(), n % 10);
        n /= 10;
    }
    return v;
}

int main(void) {
    const int input = 704321;
    RecipeList rs = RecipeList();
    rs.solve_1(input);
    rs.solve_2(input);
    return 0;
}
