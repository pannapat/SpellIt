class User():

    def __init__(self):
        self.username = None
        self.paradigm_name = None
        self.lang_name = None
        self.word_data = None
        self.word = 'Word'
        self.skeleton = 'Skeleton'
        pass


##      admin:{English:
    #                   {Noun:
    #                           Words:{walk:
    #                                   [walks, walking, walked],
    #                                  run:
    #                                   [runner, running, runs]
    #                           }

    #                           Skeleton: {Noun:
    #                               [root, singular, plural]
    #                           }
    #                       },
    #          }




    def validate_user(self):
        pass

    ################################################################    GETTERS     ############################################################
    def get_user_languages(self):

        #       data[user].keys()
        pass

    def get_user_paradigms(self, lang_name):
        self.lang_name = lang_name
        pass

    def get_user_paradigm_words(self):
        pass

    def get_user_paradigm_words_data(self):
        pass

    def mapper(self):
        pass


    ################################################################    SETTERS     ############################################################
    def set_user_language(self):
        pass

    def set_user_paradigm(self):
        pass

    def set_user_paradigm_words(self):
        pass

    def set_user_paradigm_words_data(self):
        pass