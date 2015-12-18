from pandas import HDFStore


class HDFStoreHelper:

    def __init__(self, path):
        self.path = path

    def __getitem__(self, item):
        return self._get(item)

    def __setitem__(self, key, value):
        self._put(key, value)

    def _put(self, path, obj):
        s = HDFStore(self.path)
        if path in s:
            print("updating %s" % path)
            s.remove(path)
            s.close()
        s = HDFStore(self.path)
        s[path] = obj
        s.flush(fsync=True)
        s.close()

    def _get(self, path):
        s = HDFStore(self.path)
        d = None
        if path in s:
            d = s[path]
        s.close()
        return d

    def in_store(self, path):
        s = HDFStore(self.path)
        val = False
        if path in s:
            val = True
        s.close()
        return val

    def remove(self, path):
        s = HDFStore(self.path)
        if path in s:
            print("removing %s" % path)
            s.remove(path)
            s.flush(fsync=True)
        s.close()

    def get_children_paths(self, node_path):
        s = HDFStore(self.path)
        node = s.get_node(node_path)
        children = []
        for child, df in node._v_children.items():
            children.append(df._v_pathname)
        s.close()
        return children

    def get_group_names(self):
        s = HDFStore(self.path)
        names = s.keys()
        s.close()
        return names
