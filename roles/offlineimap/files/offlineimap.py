import binascii
import codecs
from subprocess import check_output

# encrypting
def get_pass(account):
    return check_output("pass %s | head -n 1" % (account), shell=True).rstrip().decode("utf-8")
