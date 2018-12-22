/*
  type User =
    { username :: Maybe String
    , firstname :: Maybe String
    , lastname :: Maybe String
    }
 */
public class User implements Seralizable {
    protected String username;
    protected String firstname;
    protected String lastname;

    // Hibernate persistent class requires a no-argument constructor.
    public User() {}

    // naming convention:
    // getter = get + Fieldname (capitalized initial)
    // setter = set + ...
    // Hibernate discovers getters and setters by reflecting and parsing method names.
    public String getUsername() { return username; }
    // Hibernate's dirty checking relies on value comparison rather than reference identity (except collections).
    public void setUsername(String username) { this.username = username; }

    public String getFullName() {
        return firstname + " " + lastname;
    }

    public void setFullName() {
        StringTokenizer t = new StringTokenizer(name);
        firstname = t.nextToken();
        lastname = t.nextToken();
    }

}
