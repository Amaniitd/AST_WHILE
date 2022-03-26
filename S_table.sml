structure STABLE =
struct
    val s_table : (string, int) HashTable.hash_table =
        HashTable.mkTable (HashString.hashString, op=) (100, Fail "ERROR")

    fun save_value(key, value) = 
        HashTable.insert s_table (key, value);

    fun find_value(key) = HashTable.find s_table key;

    fun delete_key(key) = HashTable.remove s_table key;
end



