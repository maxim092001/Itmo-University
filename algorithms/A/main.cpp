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
const ll MAXN = 1e7 + 1;
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

int a[MAXN], c[2 * MAXN];
ll prefS[MAXN];
int main() {

    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);


    int n, x, y;
    cin >> n >> x >> y >> a[0];
    prefS[0] = a[0];

    for (int i = 1; i < n; ++i) {
        a[i] = ((x * a[i - 1]) % (1 << 16) + y) % (1 << 16);
        prefS[i] = prefS[i - 1] + a[i];
    }

    int m, z, t, b0;
    cin >> m >> z >> t >> b0;

    c[0] = b0 < 0 ? ((1 << 30) + b0) % n : (b0 % (1 << 30)) % n;

    for (int i = 1; i < 2 * m; i++) {
        b0 = (z * b0 % (1 << 30)) + t;
        c[i] = b0 < 0 ? ((1 << 30) + b0) % n : (b0 % (1 << 30)) % n;
    }

    ll ans = 0;
    for (int i = 0; i < m; ++i) {
        ans += prefS[max(c[2 * i], c[2 * i + 1])] - prefS[min(c[2 * i], c[2 * i + 1])] + a[min(c[2 * i], c[2 * i + 1])];
    }
    cout << ans;
    // 1 2 3 4 5
    // 1 3 6 10 15
    // 4 - 10
    // 2 - 3
    //
    return 0;
}