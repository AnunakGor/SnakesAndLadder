#include <bits/stdc++.h>

using namespace std;
int f(vector < vector < char >> & board) {
    vector < char > moves(101, 0);
    int b = 0;
    int i = 1;
    while (i <= 100) {
        if (b % 2 == 0) {
            for (int j = 0; j < 10; j++) {
                moves[i++] = board[10 - b - 1][j];
            }
        }
        else {
            for (int j = 9; j >= 0; j--) {
                moves[i++] = board[10 - b - 1][j];
            }
        }
        b++;
    }
    // for (auto itr: moves) cout << itr << " ";
    // cout << endl;
    //bfs
    vector < int > vis(101, 0);
    queue < int > q;
    vis[1] = 1;
    q.push(1);
    int cnt = 0;
    while (!q.empty()) {
        int level = q.size();

        while (level--) {
            int curr = q.front();
            q.pop();
            if (moves[curr] != '.') curr = moves[curr];
            if (curr == (100)) {
                return cnt;
            }

            for (int j = 1; j <= 6; j++) {
                if ((curr + j <= 100) && (vis[curr + j] == 0)) {
                    q.push(curr + j);
                    vis[curr + j] = 1;
                }
            }
            // cout << endl;
        }
        cnt++;
    }
    return -1;
}
int main() {
    vector < vector < char >> arr(10, vector < char > (10, '.'));
    for (int i = 0; i < 10; i++) {
        for (int j = 0; j < 10; j++) {
            cin >> arr[i][j];
        }
    }
    int res = f(arr);
    cout << res << endl;

    return 0;
}