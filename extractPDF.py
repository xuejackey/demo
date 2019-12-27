import json
import boto3
import time
import urllib.parse

def startJob(s3BucketName, objectName):
    response = None
    client = boto3.client('textract')
    response = client.start_document_text_detection(
    DocumentLocation={
        'S3Object': {
            'Bucket': s3BucketName,
            'Name': objectName
        }
    })

    return response["JobId"]

def isJobComplete(jobId):
    time.sleep(5)
    client = boto3.client('textract')
    response = client.get_document_text_detection(JobId=jobId)
    status = response["JobStatus"]
    print("Job status: {}".format(status))

    while(status == "IN_PROGRESS"):
        time.sleep(5)
        response = client.get_document_text_detection(JobId=jobId)
        status = response["JobStatus"]
        print("Job status: {}".format(status))

    return status

def getJobResults(jobId):

    pages = []

    time.sleep(5)

    client = boto3.client('textract')
    response = client.get_document_text_detection(JobId=jobId)
    
    pages.append(response)
    print("Resultset page recieved: {}".format(len(pages)))
    nextToken = None
    if('NextToken' in response):
        nextToken = response['NextToken']

    while(nextToken):
        time.sleep(5)

        response = client.get_document_text_detection(JobId=jobId, NextToken=nextToken)

        pages.append(response)
        print("Resultset page recieved: {}".format(len(pages)))
        nextToken = None
        if('NextToken' in response):
            nextToken = response['NextToken']

    return pages
    
def saveFile(res, documentName):
    s3BucketName="result4poc"
    documentName += ".json"
    
    s3_client = boto3.client('s3')
    response = s3_client.put_object(Bucket=s3BucketName, Key=documentName, Body=json.dumps(res))
    status_code = response['ResponseMetadata']['HTTPStatusCode']
    print(status_code)
    
    #for resultPage in response:
    #    for item in resultPage["Blocks"]:
    #        if item["BlockType"] == "LINE":
    #            print ('\033[94m' +  item["Text"] + '\033[0m')

def lambda_handler(event, context):
    
    s3BucketName = event['Records'][0]['s3']['bucket']['name']
    documentName = urllib.parse.unquote_plus(event['Records'][0]['s3']['object']['key'], encoding='utf-8')

    print("Started job with bucket: {}, and file name: {}".format(s3BucketName, documentName))
    
    #s3BucketName="upload4test"
    # AZ Tralokinumab.pdf
    #documentName="NCT02995733.pdf"
    client = boto3.client('textract')
    
    jobId = startJob(s3BucketName, documentName)
    print("Started job with id: {}".format(jobId))
    if(isJobComplete(jobId)):
        response = getJobResults(jobId)

    #print(response)
    
    saveFile(response, documentName)

    # Print detected text
    #for resultPage in response:
    #    for item in resultPage["Blocks"]:
    #        if item["BlockType"] == "LINE":
    #            print ('\033[94m' +  item["Text"] + '\033[0m')
    #            
    #process using S3 object
    #response = client.start_document_text_detection(
    #DocumentLocation={
    #    'S3Object': {
    #        'Bucket': bucket,
    #        'Name': document
    #    }
    #})
    
    #Get the text blocks
    #blocks=response["JobId"]
    #blocks=isJobComplete("5b8d38e7944f60d1bbe9310392e32c0a6ffda431ba7bbf0b31c3b35457b37318")
    
    #return {
    #    'statusCode': 200,
    #    'body': json.dumps(blocks)
    #}