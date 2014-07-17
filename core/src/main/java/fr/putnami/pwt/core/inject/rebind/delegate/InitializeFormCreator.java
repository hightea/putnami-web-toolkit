package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JField;
import com.google.gwt.core.ext.typeinfo.JParameterizedType;
import com.google.gwt.core.ext.typeinfo.JType;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.model.rebind.ModelCreator;

public class InitializeFormCreator extends InjectorCreatorDelegate {

	private final JField modelField;
	private final JType fieldType;
	private final JClassType beanType;
	private String modelImplClass;
	private Class<? extends ConstantsWithLookup> constantClassName;

	public InitializeFormCreator(JField modelField) {
		this.modelField = modelField;
		this.fieldType = modelField.getType();

		Initialize initializeAnnotation = modelField.getAnnotation(Initialize.class);
		constantClassName = initializeAnnotation.constantsClass();
		if(ConstantsWithLookup.class.equals(constantClassName)){
			constantClassName = null;
		}
		if (this.fieldType instanceof JParameterizedType) {
			JParameterizedType paramType = (JParameterizedType) fieldType;
			beanType = paramType.getTypeArgs()[0];
		}
		else {
			throw new RuntimeException("modelField can not be injected as Model");
		}
	}

	@Override
	public int getOrder() {
		return 2;
	}

	@Override
	public void create(TreeLogger logger, GeneratorContext context) {
		ModelCreator modelCreator = new ModelCreator(beanType);
		modelImplClass = modelCreator.create(logger, context);
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(MessageHelper.class.getName());
		composerFactory.addImport(ConstantsWithLookup.class.getName());
		composerFactory.addImport(beanType.getQualifiedSourceName());
	}

	@Override
	public void writeConstructor(SourceWriter srcWriter) {
		String fieldName = modelField.getName();
		if (constantClassName != null) {
			srcWriter.println("MessageHelper %sMessageHelper = new MessageHelper((ConstantsWithLookup) GWT.create(%s.class));"
					, fieldName, constantClassName.getCanonicalName());
			srcWriter.println("%s.setMessageHelper(%sMessageHelper);"
					, fieldName, fieldName);
		}
		srcWriter.println("%s.initialize(new %s());", fieldName, modelImplClass);
	}
}
