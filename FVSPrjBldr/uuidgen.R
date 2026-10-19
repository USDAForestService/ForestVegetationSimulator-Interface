uuidgen <- function (n=1)
{
# generate a version 4 uuid using a cryptographically secure random number generator (openssl::rand_num)
# example: "36d3054f-553b-4f52-ac3f-b1a028f3dfa8"

  uuid <- vector("character",n)
  for (i in 1:n) {
    # Create 4 ramdom octets for the UUID.
    rnum = openssl::rand_num(4)
    rstr <- substring(sprintf("%.8a",rnum),5,12)
    uuid[i] <- sprintf("%s-%s-4%s%s%s-%s%s",rstr[1],substring(rstr[2],1,4),
               substring(rstr[2],5,7),
               c("-8","-9","-a","-b","-8")[as.integer(rnum[1]*4)+1],
               substring(rstr[3],1,3),substring(rstr[3],4,7),rstr[4])
  }
  uuid
}
