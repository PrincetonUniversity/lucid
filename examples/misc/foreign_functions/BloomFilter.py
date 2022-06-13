import pickle

# The Bloom Filter. A precise set data structure.
filter = set()

def add_to_filter(elt):
    filter.add(elt)

# Not called directly from Lucid
def mem_filter(elt):
    return elt in filter

def record_query(elt, lucid_response):
    query = (elt, lucid_response, mem_filter(elt))
    with open("BloomFilter_out.txt", "ab") as outfile:
        pickle.dump(query, outfile)
