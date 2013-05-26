package com.bigh.mongogen.modules

import com.google.inject.AbstractModule
import com.google.inject.multibindings.Multibinder
import com.bigh.mongogen.verifier.api.Verifier
import com.bigh.mongogen.verifier._

class VerifiersModule extends AbstractModule {

  def configure() {
    val uriBinder = Multibinder.newSetBinder(binder(), classOf[Verifier])
    uriBinder.addBinding().to(classOf[RequiredStatement])
    uriBinder.addBinding().to(classOf[FieldNameUniqueness])
    uriBinder.addBinding().to(classOf[ViewMappingValidness])
    uriBinder.addBinding().to(classOf[EntityClassName])
  }
}
