#define _FORTIFY_SOURCE 0
#pragma GCC optimize("Ofast")
#pragma GCC optimize("no-stack-protector")
#pragma GCC optimize("unroll-loops")
#pragma GCC target("sse,sse2,sse3,ssse3,popcnt,abm,mmx,tune=native")
#pragma GCC optimize("fast-math")

#include <iostream>
#include <algorithm>
#include <vector>
#include <set>
#include <unordered_map>

using namespace std;

typedef long long ll;
const ll N = 2e5 + 10;

#define x first
#define y second
#define pb push_back

int comp[N];
bool used[N];
vector<vector<int>> g, h;
vector<int> a;
int color = 1;
unordered_map<string, int> dist;
unordered_map<int, string> m1;
unordered_map<int, int> m2;

void dfs1(int v) {
    used[v] = true;

    for (auto to : g[v]) {
        if (!used[to]) {
            dfs1(to);
        }
    }
    a.pb(v);
}

void dfs2(int v) {
    comp[v] = color;

    for (auto to : h[v]) {
        if (comp[to] == 0) {
            dfs2(to);
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;

    int num = 0;
    vector<int> nums;
    for (int i = 0; i < n; i++) {
        string s;
        cin >> s;

        nums.pb(num);
        dist["+" + s] = num;
        m1[num] = "+" + s;
        m2[num] = num + 1;
        num++;

        dist["-" + s] = num;
        m1[num] = "-" + s;
        m2[num] = num - 1;
        num++;
    }

    g.resize(2 * n);
    h.resize(2 * n);

    for (size_t i = 0; i < m; i++) {
        string s, w;
        cin >> s >> w >> w;

        int x = dist[s];
        int y = dist[w];

        g[x].pb(y);
        g[m2[y]].pb(m2[x]);

        h[y].pb(x);
        h[m2[x]].pb(m2[y]);
    }


    for (size_t i = 0; i < g.size(); i++) {
        if (!used[i]) {
            dfs1(i);
        }
    }

    for (int i = a.size() - 1; i >= 0; i--) {
        if (comp[a[i]] == 0) {
            dfs2(a[i]);
            color++;
        }
    }

    set<string> tusovo4ka;

    for (auto it : nums) {
        if (comp[it] == comp[m2[it]]) {
            return cout << -1, 0;
        } else if (comp[it] > comp[m2[it]]) {
            tusovo4ka.insert(m1[it]);
        }
    }

    if (tusovo4ka.empty()) {
        return cout << -1, 0;
    }

    cout << tusovo4ka.size() << endl;
    for (const auto &it : tusovo4ka) {
        cout << it.substr(1) << endl;
    }
}