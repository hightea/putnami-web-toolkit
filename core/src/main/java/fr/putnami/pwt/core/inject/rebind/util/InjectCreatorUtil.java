package fr.putnami.pwt.core.inject.rebind.util;

import java.lang.annotation.Annotation;
import java.util.Collection;

import com.google.common.collect.Lists;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JField;
import com.google.gwt.core.ext.typeinfo.JMethod;

public final class InjectCreatorUtil {

	public static Collection<JMethod> listMethod(JClassType type, Class<? extends Annotation> annotationClass) {
		Collection<JMethod> methodAnnoted = Lists.newArrayList();
		JMethod[] methods = type.getOverridableMethods();
		for (JMethod method : methods) {
			Annotation annotation = method.getAnnotation(annotationClass);
			if (annotation != null) {
				methodAnnoted.add(method);
			}
		}

		return methodAnnoted;
	}

	public static Collection<JField> listFields(JClassType type, Class<? extends Annotation> annotationClass) {
		Collection<JField> methodAnnoted = Lists.newArrayList();
		JField[] fields = type.getFields();
		for (JField field : fields) {
			Annotation annotation = field.getAnnotation(annotationClass);
			if (annotation != null) {
				methodAnnoted.add(field);
			}
		}
		return methodAnnoted;
	}

	private InjectCreatorUtil() {

	}
}
