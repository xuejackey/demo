import json
import urllib.parse
import boto3
import decimal

print('Loading function')

s3 = boto3.client('s3')
db = boto3.resource('dynamodb')

def getText(jsonfile):
    str = ""
        
    for resultPage in jsonfile:
        for item in resultPage["Blocks"]:
            if item["BlockType"] == "LINE":
                str += item["Text"] + '\r\t'
    
    #print(str)
    return str

def save2s3(jsonfile, documentname):
    str = getText(jsonfile)
    
    responseS3 = s3.put_object(Bucket='result4txt', Key=documentname, Body=str)
    
def save2DynamoDB(key, text):
    print(key)
    table = db.Table('protocol')
    
    try:
        currentStats = table.get_item(
            Key = {
                'id': key
            }
        )
    except Exception as e:
        print(e)
        return e
    # {currentStats.keys()}
    print(f'currentStats: ')
    
    if 'Item' not in currentStats.keys():
        statItem = {
            'id': key,
            'blocks': text
        }
    #{statItem.id}
    print(f'New Record: {statItem["id"]}')
        
    try:
        print(f'New Record1: ')
        table.put_item(Item=statItem)
    except Exception as e:
        print(e)
        return e


def lambda_handler(event, context):
    #print("Received event: " + json.dumps(event, indent=2))

    # Get the object from the event and show its content type
    bucket = event['Records'][0]['s3']['bucket']['name']
    key = urllib.parse.unquote_plus(event['Records'][0]['s3']['object']['key'], encoding='utf-8')
    try:
        response = s3.get_object(Bucket=bucket, Key=key)
        print("CONTENT TYPE: " + response['ContentType'])
        jsonfile = json.load(response['Body'], parse_float = decimal.Decimal)
        #print(jsonfile)
        save2s3(jsonfile, key+".txt")
        save2DynamoDB(key, getText(jsonfile))
        return response['ContentType']
    except Exception as e:
        print(e)
        print('Error getting object {} from bucket {}. Make sure they exist and your bucket is in the same region as this function.'.format(key, bucket))
        raise e
