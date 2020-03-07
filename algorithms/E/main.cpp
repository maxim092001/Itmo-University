#pragma comment(linker, "/stack:200000000")
#pragma GCC optimize("Ofast")
#pragma GCC optimize("unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,sse4,popcnt,abm,mmx,avx,tune=native")
#pragma GCC optimize("no-stack-protector")
#pragma GCC optimize("fast-math")


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

const ll INF = 1e18 + 1;
const int INF_INT = 2e9 + 100;
const ll POLTOS = 50;
const ll SOTKA = 100;
const ll P = 1e9 + 7;
const ld eps = 1e-11;
const ll base = 1000 * 1000 * 1000;
const ll M = 3050;
const ll MAXN = 1e6 + 1;
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

struct matrix {
    matrix(int x, int y, int x1, int y1) : x(x), y(y), x1(x1), y1(y1) {}
    matrix() {}

    int x, y;
    int x1, y1;
};

matrix t[MAXN * 4];
int r;


matrix mult_matrix(matrix a, matrix b) {
    matrix res;
    res.x = ((a.x * b.x) % r + (a.y * b.x1) % r) % r;
    res.y = ((a.x * b.y) % r + (a.y * b.y1) % r) % r;
    res.x1 = ((a.x1 * b.x) % r + (a.y1 * b.x1) % r) % r;
    res.y1 = ((a.x1 * b.y) % r + (a.y1 * b.y1) % r) % r;
    return res;
}

void build(matrix a[], int left, int right, int v) {
    if (right - left == 1) {
        t[v] = a[left];
        return;
    }
    int mid = (left + right) / 2;
    build(a, left, mid, v * 2 + 1);
    build(a, mid, right, v * 2 + 2);
    t[v] = mult_matrix(t[v * 2 + 1], t[v * 2 + 2]);
}

matrix get_sum(int v, int left, int right, int tree_left, int tree_right) {
    if (left >= tree_right || tree_left >= right) {
        return matrix(1, 0, 0, 1);
    }
    if (tree_left >= left && tree_right <= right) {
        return t[v];
    }
    int mid = (tree_left + tree_right) / 2;
    return mult_matrix(get_sum(v * 2 + 1, left, right, tree_left, mid),
                       get_sum(v * 2 + 2, left, right, mid, tree_right));
}

int main() {

    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);
    int n, m;
    cin >> r >> n >> m;

    matrix a[n];
    for (int i = 0; i < n; ++i) {
        cin >> a[i].x >> a[i].y >> a[i].x1 >> a[i].y1;
    }

    build(a, 0, n, 0);
    for (int i = 0; i < m; ++i) {
        int left, right;
        cin >> left >> right;
        left--;
        matrix res = get_sum(0, left, right, 0, n);
        cout << res.x << ' ' << res.y << '\n';
        cout << res.x1 << ' ' << res.y1 << '\n';
        cout << '\n';
    }
    return 0;
}