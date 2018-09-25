# -*- coding: utf-8 -*-
import os
from flask import Flask, jsonify, request, json
from flask_restful import Resource, Api, reqparse
from flask_cors import CORS
from Processing_engine import Processing_engine
from CouchAPI import CouchAPI


app = Flask(__name__)

CORS(app)

api = Api(app)

global couchOBJ
global engineOBJ


engineOBJ = Processing_engine()
couchOBJ = CouchAPI('Administrator', 'password', 'localhost')


class AuthenticateUser(Resource):

    def post(self):

        request_data = json.loads(request.data.decode())
        engine = globals()['engineOBJ']
        return engine.verify_user(request_data)


class GetCurrentUser(Resource):

    def post(self):
        return globals()['user']


class SaveData(Resource):

    def post(self):

        request_data = json.loads(request.data.decode())
        engine = globals()['engineOBJ']
        return engine.save_data(request_data)

class RetrieveLangNames(Resource):

    def post(self):

        engine = globals()['engineOBJ']
        return engine.retrieve_lang_names()

class RetrieveLangData(Resource):

    def post(self):

        request_data = json.loads(request.data.decode())
        engine = globals()['engineOBJ']
        return engine.retrieve_lang_data(request_data)



api.add_resource(RetrieveLangData, '/retrieve_data')
api.add_resource(RetrieveLangNames,'/retrieve_langs')
api.add_resource(AuthenticateUser,'/auth')
api.add_resource(SaveData,'/save_data')

if __name__ == '__main__':
    app.run(debug=True, host='127.0.0.1',threaded = True)
