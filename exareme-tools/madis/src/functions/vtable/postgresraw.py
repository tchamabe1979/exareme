"""
.. function:: postgres(host, port, user, passwd, db, query:None)

Connects to an PostgreSQL DB and returns the results of query.

Examples:

    >>> sql("select * from (postgres h:127.0.0.1 port:5432 u:root p:rootpw db:testdb select 5 as num, 'test' as text);")
    num | text
    -----------
    5   | test

"""

import setpath
import vtbase
import functions

import json
from types import NoneType

registered = True
external_query = True

class PostgresRaw(vtbase.VT):
    def VTiter(self, *parsedArgs,**envars):
        # DBAPI.warn = lambda x,stacklevel:x

        typetrans = {
            16: 'INT',
            17: 'INT',
            19: 'TEXT', # name type
            20: 'INT',
            21: 'INT',
            23: 'INT',
            25: 'TEXT', # TEXT type
            26: 'INT', # oid type
            142: 'TEXT', # XML
            194: 'TEXT', # "string representing an internal node tree"
            700: 'REAL',
            701: 'REAL',
            705: 'TEXT',
            829: 'TEXT', # MACADDR type
            1000: 'INT', # BOOL[]
            1003: 'TEXT', # NAME[]
            1005: 'INT', # INT2[]
            1007: 'INT', # INT4[]
            1009: 'TEXT', # TEXT[]
            1014: 'TEXT', # CHAR[]
            1015: 'TEXT', # VARCHAR[]
            1016: 'INT', # INT8[]
            1021: 'REAL', # FLOAT4[]
            1022: 'REAL', # FLOAT8[]
            1042: 'TEXT', # CHAR type
            1043: 'TEXT', # VARCHAR type
            1082: 'TEXT',
            1083: 'TEXT',
            1114: 'TEXT',
            1184: 'TEXT', # timestamp w/ tz
            1186: '',
            1231: '', # NUMERIC[]
            1263: 'TEXT', # cstring[]
            1700: '',
            2275: 'TEXT', # cstring
        }

        largs, dictargs = self.full_parse(parsedArgs)

        #if 'query' not in dictargs:
        #   raise functions.OperatorError(__name__.rsplit('.')[-1],"No query argument ")

        query=dictargs['query']
        
	# get site properties
        data = json.load(open("/root/mip-algorithms/properties.json"))
        for d in data["local_engine_default"]["parameters"]:
            dictargs[d["name"]] = d["value"]
        query = data["local_engine_default"]["query"]

        host = str(dictargs.get('host', dictargs.get('h', '127.0.0.1')))
        port = int(dictargs.get('port', 5432))
        user = str(dictargs.get('username', dictargs.get('u', '')))
        passwd = str(dictargs.get('password', dictargs.get('p', '')))
        db = str(dictargs.get('db', ''))
        query=query.replace('%','%%')

        import psycopg2
        try:
            conn = psycopg2.connect(user=user, host=host, port=port, database=db, password=passwd)

            cur = conn.cursor()
            cur.execute(query)

            yield [(c[0], typetrans.get(c[1], '')) for c in cur.description]

            for i in cur:
                yield [unicode(c) if type(c) not in (long, int, float, str, unicode, NoneType, bool) else c for c in i]

            cur.close()
        except Exception, e:
            raise functions.OperatorError(__name__.rsplit('.')[-1], ' '.join(str(t) for t in e))

def Source():
    return vtbase.VTGenerator(PostgresRaw)


if not ('.' in __name__):
    """
    This is needed to be able to test the function, put it at the end of every
    new function you create
    """
    import sys
    import setpath
    from functions import *
    testfunction()
    if __name__ == "__main__":
        reload(sys)
        sys.setdefaultencoding('utf-8')
        import doctest
        doctest.testmod()
