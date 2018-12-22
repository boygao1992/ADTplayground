import javax.persistent.Entity;

/*
  type Item =
  { name :: Maybe String
  , bids :: Set Bid
  }
*/
@Entity
@org.hibernate.annotations.Cache (
    usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE
)
public class Item {
    protected String name;
    protected Set<Bid> bids = new HashSet<Bid>();

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public Set<Bid> getBids() { return bids; }
    public void setBids(Set<Bid> bids) { this.bids = bids; }

    public void addBid(Bid bid) {
        if (bid == null)
            throw new NullPointerException("Can't add null Bid");
        if (bid.getItem() != null)
            throw new IllegalStateException("Bid is already assigned to an Item");
        getBids().add(bid);
        bid.setItem(this); // tying the knot, bidirectional edge
    }
}
