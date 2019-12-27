import json
import boto3
import requests
import decimal
from requests_aws4auth import AWS4Auth

region = 'us-east-2' # e.g. us-west-1
service = 'es'
credentials = boto3.Session().get_credentials()
awsauth = AWS4Auth(credentials.access_key, credentials.secret_key, region, service, session_token=credentials.token)

# es config
host = 'https://search-lillypdf-e3zyugi67iyxcllrznlnyoc5py.us-east-2.es.amazonaws.com' # the Amazon ES domain, including https://
index = 'jsonindexs'
type = 'jsontype'
url = host + '/' + index + '/' + type

headers = { "Content-Type": "application/json" }

def lambda_handler(event, context):
    # read from s3 bucket
    # get s3 client
    s3 = boto3.client('s3')
    
    for record in event['Records']:
        # Get the bucket name and key for the new file
        bucket = record['s3']['bucket']['name']
        key = record['s3']['object']['key']
        
        # Get, read, and split the file into lines
        obj = s3.get_object(Bucket=bucket, Key=key)
        jsonfile = json.load(obj['Body'], parse_float = decimal.Decimal)
        
        content_str = ""
        for resultPage in jsonfile:
            for item in resultPage["Blocks"]:
                if item["BlockType"] == "LINE" : #or item['BlockType'] == 'WORD':
                    content_str += item["Text"] + '\r\t'
        
        content = {"content": content_str}
        r = requests.post(url, auth=awsauth, json=content, headers=headers)
    
    return {
        'statusCode': 200,
        'body': json.dumps('Finished load data into ES!')
    }
