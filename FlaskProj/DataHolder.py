from Processing_engine import Processing_engine
from CouchAPI import CouchAPI

class DataHolder():

    def __init__(self):
        self.engine_obj = Processing_engine()
        self.couch_obj = CouchAPI('Administrator', 'password', 'localhost')

