package cc.factorie.app.nlp.xcoref

/**
 * @author John Sullivan
 */

/*
Canopies are basically bins or sets of elements, used to access
a relevent subset in a large set of elements.

The idea here is that an element that mixes in canopization
must know how to generate the canopies it belongs to
(thereby allowing it to access the other members of its canopies).
 */
object NodeStringCanopyStore {
  private val _map = new collection.mutable.HashMap[String, collection.mutable.Set[Node[_] with NodeStringCanopization]]
  def retrieve(canopyId: String) = _map.getOrElse(canopyId, collection.mutable.Set.empty[Node[_] with NodeStringCanopization])
  def save(canopyId: String, member: Node[_] with NodeStringCanopization) = _map.getOrElseUpdate(canopyId, collection.mutable.Set.empty[Node[_] with NodeStringCanopization]) += member
}

trait NodeStringCanopization {
  self: Node[_] with NodeStringCanopization =>

  hashAndAddToCanopy
  def canopyIds: Set[String]
  def membersOfCanopies = canopyIds.map(canopyId => NodeStringCanopyStore.retrieve(canopyId)).toSet
  private def hashAndAddToCanopy = {
    val canopies = this.canopyIds
    canopies.foreach(canopyId => NodeStringCanopyStore.save(canopyId, this))
  }
}

/*
trait CanopyIndex[Member, Canopy] {
  def retrieve(c:Canopy):Set[Member]
  def save(member:Member, canopy:Canopy):Unit
}


trait Canopization[Member, Canopy] {
  self: Member =>
  def canopyHashes:Set[Canopy]

  protected val canopyStore:CanopyIndex[Member, Canopy]
  def addToCanopy(member:Member, canopy:Canopy) {canopyStore.save(member, canopy)}

  private def hashAndAddToCanopy:Set[Canopy] = {
    val canopies = canopyHashes
    canopies.foreach{ canopy =>
      addToCanopy(this, canopy)
    }
    canopies
  }

  // This doesn't support a streaming model
  lazy val canopies:Set[Set[Member]] = hashAndAddToCanopy.map(canopy => canopyStore.retrieve(canopy))

}
*/