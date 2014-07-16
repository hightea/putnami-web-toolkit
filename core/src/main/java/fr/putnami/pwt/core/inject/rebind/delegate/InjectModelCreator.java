package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JField;
import com.google.gwt.core.ext.typeinfo.JParameterizedType;
import com.google.gwt.core.ext.typeinfo.JType;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.rebind.ModelCreator;

public class InjectModelCreator extends InjectorCreatorDelegate {

	private final JField modelField;
	private final JType fieldType;
	private final JClassType beanType;

	private String modelImplClass;

	public InjectModelCreator(JField modelField) {
		this.modelField = modelField;
		this.fieldType = modelField.getType();

		if (this.fieldType instanceof JParameterizedType) {
			JParameterizedType paramType = (JParameterizedType) fieldType;
			beanType = paramType.getTypeArgs()[0];
		}
		else {
			throw new RuntimeException("modelField can not be injected as Model");
		}

	}

	@Override
	public void create(TreeLogger logger, GeneratorContext context) {
		ModelCreator modelCreator = new ModelCreator(beanType);
		modelImplClass = modelCreator.create(logger, context);
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(Model.class.getName());
		composerFactory.addImport(beanType.getQualifiedSourceName());

	}

	@Override
	public void writeConstructor(SourceWriter srcWriter) {
		srcWriter.println("%s = new %s();", modelField.getName(), modelImplClass);
	}
}
