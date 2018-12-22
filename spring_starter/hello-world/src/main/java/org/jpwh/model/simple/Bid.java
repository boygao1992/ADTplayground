/*
  type Bid =
    { item :: Maybe Item
    }
 */
public class Bid {
    protected Item item;

    public Bid(Item item) {
        this.item = item;
        item.getBids().add(this); // tying the knot, bidirectional edge
    }

    public Item getItem() { return item; }
    public void setItem(Item item) { this.item = item; }

}
