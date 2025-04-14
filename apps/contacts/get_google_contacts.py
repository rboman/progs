#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# Ce script utilise l'API Google People pour récupérer les contacts de l'utilisateur.
#
# python3 -m venv .venv
# source .venv/bin/activate
# pip install google-api-python-client google-auth-httplib2 google-auth-oauthlib


import os
import pickle
import json
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request
from googleapiclient.discovery import build

credentials_file = '/hdd2/boman/Dropbox/Bin/google_api_credentials.json'

# On définit la portée (scope) en mode lecture seule pour accéder aux contacts.
SCOPES = ['https://www.googleapis.com/auth/contacts.readonly']

def main():
    creds = None
    # Vérifier si un token d'accès existe déjà dans token.pickle
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
    
    # Si aucun token n'existe ou si celui-ci est expiré, on initie le processus d'authentification.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            # On rafraîchit le token
            creds.refresh(Request())
        else:
            # On lance le flux OAuth2.0 pour récupérer les identifiants
            flow = InstalledAppFlow.from_client_secrets_file(credentials_file, SCOPES)
            creds = flow.run_local_server(port=0)
        # Sauvegarde des identifiants dans token.pickle pour les utilisations futures.
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)
    
    # Construction du service API Google People
    service = build('people', 'v1', credentials=creds)
    
    # Récupération des contacts
    results = service.people().connections().list(
        resourceName='people/me',
        personFields='names,emailAddresses'
    ).execute()
    
    # Extraction de la liste des contacts
    connections = results.get('connections', [])
    
    # Sauvegarde des contacts dans un fichier JSON
    with open("google_contacts.json", "w", encoding="utf-8") as f:
        json.dump(connections, f, ensure_ascii=False, indent=4)
    
    print(f"{len(connections)} contacts ont été enregistrés dans 'google_contacts.json'.")

if __name__ == '__main__':
    main()
