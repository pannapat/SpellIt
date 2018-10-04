from CouchAPI import CouchAPI

global couchOBJ
global user





class Processing_engine():

    def __init__(self):
        self.couch = globals()['couchOBJ']


    def verify_user(self, request_data):

        username = request_data['user']
        password = request_data['pass']

        print(username)

        self.couch.open_bucket('auth')
        result = self.couch.authenticate(username, password)

        if result == True:
            global user
            user = username

        else:
            print('invalid user')
            return {'Error': 'Unable to authenticate user "'+username+'"'}

        return result


    def save_data(self, request_data):

        try:

            words_data = {}
            meta = request_data['meta']
            data = request_data['data']
            print(globals()['user'])
            username = globals()['user']

            # Creating Key, Value pairs of the data
            for x in data:
                if not x['root_data']:
                    continue
                root_words = filter(None, x['root_data'].split('\n'))
                words_data[x['root_word']] = root_words

            if len(words_data) <= 0:
                return
                # (Key)Language Name : Value ->(Tuple) (Language Dictionary, Language Symbol)
            final_data = {meta['lang_name']: (words_data, meta['lang_symbol'])}

            # Get the global couch db object
            couch = globals()['couchOBJ']

            # Open data bucket and store data
            couch.open_bucket('data')

            try:
                user_data = couch.retrieve_data(username).value
                user_data[meta['lang_name']] = (words_data, meta['lang_symbol'])
                couch.replace_data(username, user_data)
            except:
                couch.store_data(username, final_data)

            return True

        except:
            raise

    def retrieve_lang_names(self):

        try:

            username = globals()['user']
            print username
            self.couch.open_bucket('data')
            user_data = self.couch.retrieve_data(username).value
            user_langs = list(user_data.keys())
            return user_langs

        except:
            raise


    def retrieve_lang_data(self, lang_name):

        try:

            username = globals()['user']
            self.couch.open_bucket('data')
            lang_name = lang_name['lang']
            user_data = self.couch.retrieve_data(username).value
            lang_data = user_data[lang_name]
            return lang_data

        except:
            raise



couchOBJ = CouchAPI('Administrator', 'password', 'localhost')
# user = 'admin'
# e = Processing_engine()
# lang_name = e.retrieve_lang_names()
# print lang_name
# print e.retrieve_lang_data(lang_name[0])[0]