from couchbase.cluster import Cluster, PasswordAuthenticator


cluster = Cluster('couchbase://localhost')
cluster.authenticate(PasswordAuthenticator('Administrator', 'password'))
bucket = cluster.open_bucket('auth')

bucket.upsert('admin', 'admin')