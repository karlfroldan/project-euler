#include <iostream>
#include <queue>
#include <vector>

long long count_lattice_paths(int);

int main() {
    long long s;
    int c;
    std::cin >> c;
    s = count_lattice_paths(c);

    std::cout << s << "\n";

    return 0;
}

long long aux_func(long long** count_matrix, int i, int j) {
    if (count_matrix[i][j] == 0) {
        count_matrix[i][j] = aux_func(count_matrix, i - 1, j) + aux_func(count_matrix, i, j - 1);
    } 

    return count_matrix[i][j];
}


long long count_lattice_paths(int n) {

    // This matrix counts exactly how many lines are using that certain point.
    auto count_matrix = new long long*[n + 1];
    
    for (int i = 0; i < n + 1; ++i) {
        count_matrix[i] = new long long[n + 1];
    }

    for (int i = 0; i < n + 1; ++i) {
        for (int j = 0; j < n + 1; ++j) {
            count_matrix[i][j] = 0;
        }
    }

    // Fill the upper and left borders with 0s.
    for (int i = 0; i < n + 1; ++i) {
        count_matrix[i][0] = 1;
        count_matrix[0][i] = 1;
    }

    count_matrix[n][n] = aux_func(count_matrix, n, n);


    // for (int i = 0; i <= n; ++i) {
    //     for (int j = 0; j <= n; ++j) {
    //         std::cout << count_matrix[i][j] << " ";
    //     }
    //     std::cout << "\n";
    // }

    return count_matrix[n][n];
}