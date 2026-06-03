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
    # Vérifie l'existence d'un token pour l'authentification
    if os.path.exists('token.pickle'):
        with open('token.pickle', 'rb') as token:
            creds = pickle.load(token)
    
    # Si le token n'existe pas ou est expiré, lancer le flux OAuth
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(credentials_file, SCOPES)
            creds = flow.run_local_server(port=0)
        with open('token.pickle', 'wb') as token:
            pickle.dump(creds, token)
    
    # Construction du service API
    service = build('people', 'v1', credentials=creds)
    
    # Récupération de tous les contacts en utilisant la pagination
    all_connections = []
    page_token = None

    while True:
        response = service.people().connections().list(
            resourceName='people/me',
            pageSize=100,  # Nombre maximum de contacts par page (100 est la limite usuelle)
            pageToken=page_token,
            personFields='names,emailAddresses'
        ).execute()

        connections = response.get('connections', [])
        all_connections.extend(connections)
        
        # Récupérer le jeton de la page suivante
        page_token = response.get('nextPageToken')
        if not page_token:
            break

    # Sauvegarde de tous les contacts dans un fichier JSON
    with open("google_contacts.json", "w", encoding="utf-8") as f:
        json.dump(all_connections, f, ensure_ascii=False, indent=4)
    
    print(f"{len(all_connections)} contacts ont été enregistrés dans 'google_contacts.json'.")

if __name__ == '__main__':
    main()
