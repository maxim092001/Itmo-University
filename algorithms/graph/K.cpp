#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

typedef long long ll;

const int INF = 1e9 + 100;

#define x first
#define y second
#define pb push_back

short color;
ll res = 0;

void dfs1(short v, vector<vector<short>> &g,
          vector<short> &comp, vector<short> &a, vector<bool> &used) {
    used[v] = true;

    for (auto to : g[v]) {
        if (!used[to]) {
            dfs1(to, g, comp, a, used);
        }
    }

    a.pb(v);
}

void dfs2(short v, vector<vector<short>> &g,
          vector<short> &comp) {
    comp[v] = color;


    for (auto to : g[v]) {
        if (comp[to] == -1) {
            dfs2(to, g, comp);
        }
    }
}

void condensation(vector<vector<short>> &g,
                  vector<vector<short>> &gr, short n, vector<short> &tempRes) {
    vector<short> a(n);
    vector<bool> used(n);

    for (short i = 0; i < n; i++) {
        if (!used[i]) {
            dfs1(i, g, tempRes, a, used);
        }
    }

    color = 0;
    for (short i = a.size() - 1; i >= 0; i--) {
        if (tempRes[a[i]] == -1) {
            dfs2(a[i], gr, tempRes);
            color++;
        }
    }
}

void dfs(short v, vector<vector<short>> &g, vector<bool> &used) {
    used[v] = true;

    for (auto to : g[v]) {
        if (!used[to]) {
            dfs(to, g, used);
        }
    }
}

bool checkReachability(short root, vector<vector<short>> &g, short n) {
    vector<bool> used(n, false);
    dfs(root, g, used);
    return std::all_of(used.begin(), used.end(), [](bool b){return b;});
}

struct edge {
    edge(short from, short to, int w) : from(from), to(to), w(w) {}
    short from;
    short to;
    int w;
};

void findMST(vector<edge> &edges, short n, short root) {
    int minEdge[n];
    for (short i = 0; i < n; i++) {
        minEdge[i] = INF;
    }

    for (auto e: edges) {
        minEdge[e.to] = min(e.w, minEdge[e.to]);
    }

    for (short i = 0; i < n; i++) {
        if (i == root) continue;
        res += minEdge[i];
    }

    vector<vector<short>> g(n);
    vector<vector<short>> gr(n);
    for (auto e : edges) {
        if (e.w == minEdge[e.to]) {
            g[e.from].pb(e.to);
            gr[e.to].pb(e.from);
        }
    }

    if (checkReachability(root, g, n)) {
        return;
    }

    vector<edge> newEdges;
    vector<short> newComponents(n, -1);
    condensation(g, gr, n, newComponents);

    for (auto e : edges) {
        if (newComponents[e.from] != newComponents[e.to]) {
            newEdges.emplace_back(newComponents[e.from], newComponents[e.to], e.w - minEdge[e.to]);
        }
    }

    findMST(newEdges, color, newComponents[root]);
}

void dfsMain(short v, vector<vector<short>> &g, vector<bool> &used) {
    used[v] = true;

    for (auto to : g[v]) {
        if (to == v) {
            continue;
        }

        if (!used[to]) {
            dfsMain(to, g, used);
        }
    }
}

bool checkReachability2(short root, vector<vector<short>> &g, short n) {
    vector<bool> used(n, false);
    dfsMain(root, g, used);
    return std::all_of(used.begin(), used.end(), [](bool b){return b;});
}

int main() {
    short n;
    short m;
    cin >> n >> m;
    vector<edge> edges;
    vector<vector<short>> g(n);
    for (short i = 0; i < n; i++) {
        g[i] = vector<short>();
    }

    short x, y;
    int z;
    for (short i = 0; i < m; i++) {
        cin >> x >> y >> z;
        x--, y--;
        edges.emplace_back(x, y, z);
        g[x].pb(y);
    }

    if (!checkReachability2(0, g, n)) {
        return cout << "NO", 0;
    }

    g.clear();
    cout << "YES" << endl;
    findMST(edges, n, 0);
    cout << res << endl;
}