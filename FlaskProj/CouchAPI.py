# -*- coding: utf-8 -*-
from couchbase.cluster import Cluster, PasswordAuthenticator


class CouchAPI():

    def __init__(self, username, password, ip):
        try:
            self.cluster = Cluster('couchbase://'+ip)
            self.cluster.authenticate(PasswordAuthenticator(username, password))

        except:
            raise Exception('Unable to verify user')

    def open_bucket(self, bucket_name):
        self.bucket = self.cluster.open_bucket(bucket_name)

    def authenticate(self, username, password):
        try:
            pwd = self.bucket.get(username).value

            if pwd == password:
                return True
            else:
                return False
        except:
            raise


    def store_data(self, user, data):

        try:
            self.bucket.upsert(user, data)
        except:
            raise

    def replace_data(self, user, data):

        try:
            self.bucket.replace(user, data)
        except:
            raise

    def retrieve_data(self, username):

        try:
            return self.bucket.get(username)
        except:
            raise

