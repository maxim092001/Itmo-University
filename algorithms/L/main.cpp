#include <iostream>
#include <set>
#include <map>
#include <vector>
#include <iomanip>
#include <stack>
#include <deque>
#include <queue>
#include <string>
#include <cstring>
#include <cstdio>
#include <algorithm>
#include <cmath>
#include <unordered_set>
#include <unordered_map>
#include <numeric>
#include <cassert>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef unsigned long long ull;
typedef pair<ll, ll> pll;
typedef pair<ld, ld> pld;
typedef pair<int, int> pii;

const ll INF = 1e18 + 100;
const int INF_INT = 2e9 + 100;
const ll POLTOS = 50;
const ll SOTKA = 100;
const ll P = 1e9 + 7;
const ld eps = 1e-11;
const ll base = 1000 * 1000 * 1000;
const ll M = 150;
const ll MAXN = 2e6 + 1;
const ll MAXN1 = 2 * 1e2;
const ll N = 300;
const ld pi = acos(-1);
const int mod = 1e9 + 7;
const int S_M = 20;
const ll LEN = 96;
const ll DMOD = 500000004;
const ll PR = 32000;

#define eb emplace_back
#define pb push_back
#define pf push_front
#define ppb pop_back
#define ppf pop_front
#define mp make_pair
#define ALL(a) (a).begin(), (a).end()
#define sqr(x) ((x) * (x))

int n;
ll t[M][M][M];

int F(int i) {
    return i & (i + 1);
}

void update(int x, int y, int z, int d) {
    for (int i = x; i < n; i = i | (i + 1)) {
        for (int j = y; j < n; j = j | (j + 1)) {
            for (int k = z; k < n; k = k | (k + 1)) {
                t[i][j][k] += d;
            }
        }
    }
}

ll query_sum(int x, int y, int z) {
    ll s = 0;
    for (int i = x; i >= 0; i = F(i) - 1) {
        for (int j = y; j >= 0; j = F(j) - 1) {
            for (int k = z; k >= 0; k = F(k)  - 1) {
                s += t[i][j][k];
            }
        }
    }
    return s;
}
int main() {
    // freopen("input.txt", "r", stdin);
    // freopen("output.txt", "w", stdout);


    cin >> n;

    int m = 500;
    while (m != 3) {
        cin >> m;

        if (m == 2) {
            int x1, y1, z1, x2, y2, z2;
            cin >> x1 >> y1 >> z1 >> x2 >> y2 >> z2;
            cout << query_sum(x2, y2, z2) - query_sum(x2, y1 - 1, z2) - query_sum(x1 - 1, y2, z2)
                    - query_sum(x2, y2, z1 - 1) + query_sum(x2, y1 - 1, z1 - 1) + query_sum(x1 - 1, y2, z1 - 1)
                    + query_sum(x1 - 1, y1 - 1, z2) - query_sum(x1 - 1, y1 - 1, z1 - 1) << endl;
            continue;
        }

        if (m == 1) {
            int x, y, z, d;
            cin >> x >> y >> z >> d;
            update(x, y, z, d);
            continue;
        }
    }
    return 0;
}
