# jsbsimd-server


extract localhosts tls base64 key
```
openssl s_client -connect localhost:3000 | openssl x509 -pubkey -noout | openssl rsa -pubin -outform der | openssl dgst -sha256 -binary | openssl enc -base64
```
