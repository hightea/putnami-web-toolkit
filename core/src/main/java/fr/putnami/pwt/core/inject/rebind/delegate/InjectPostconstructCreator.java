package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.core.ext.typeinfo.JMethod;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;

public class InjectPostconstructCreator extends InjectorCreatorDelegate {

	private final JMethod postConstructMethod;

	public InjectPostconstructCreator(JMethod postConstructMethod) {
		this.postConstructMethod = postConstructMethod;
	}

	@Override
	public int getOrder() {
		return LOWEST_PRECEDENCE;
	}


	@Override
	public void writeConstructor(SourceWriter srcWriter) {
		srcWriter.println("%s();", postConstructMethod.getName());
	}

}
