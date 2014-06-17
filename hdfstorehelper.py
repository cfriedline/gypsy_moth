import pandas

class HDFStoreHelper:
    def __init__(self, path):
        self.path = path

    def put(self, path, obj):
        s = pandas.HDFStore(self.path)
        if path in s:
            print "updating %s" % path
            s.remove(path)
        s[path] = obj
        s.close()
    

    def get(self, path):
        s = pandas.HDFStore(self.path)
        d = None
        if path in s:
            d = s[path]
        s.close()
        return d


    def in_store(self, path):
        s = pandas.HDFStore(self.path)
        val = False
        if path in s:
            val = True
        s.close()
        return val

    def remove(path):
        s = pandas.HDFStore(self.path)
        if path in s:
            print "removing %s" % path
            s.remove(path)
        s.close()

    def get_children_paths(self, node_path): 
        s = pandas.HDFStore(self.path)
        node = s.get_node(node_path)
        children = []
        for child, df in node._v_children.items():
            children.append(df._v_pathname)
        s.close()
        return children
    
