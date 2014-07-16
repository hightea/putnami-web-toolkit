package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.ext.typeinfo.JField;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;

public class InjectResourceCreator extends InjectorCreatorDelegate {

	private final JField resourceField;

	public InjectResourceCreator(JField resourceField) {
		this.resourceField = resourceField;
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(GWT.class.getName());

	}

	@Override
	public void writeConstructor(SourceWriter srcWriter) {
		srcWriter.println("%s = GWT.create(%s.class);", resourceField.getName(), resourceField.getType().getQualifiedSourceName());
	}
}
