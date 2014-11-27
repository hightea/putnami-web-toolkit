/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.model.client.visitor;


import com.google.common.collect.Lists;
import com.google.gwt.user.client.ui.Focusable;

import java.util.List;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorError;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.client.impl.SimpleError;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.util.ModelUtils;

public class ErrorBinderVisitor extends AbstractVisitor {

	private static final String JAVAX_VALIDATION_START = "javax.validation.constraints.";
	private static final String JAVAX_VALIDATION_START_SUBSTITUTE = "constraints";
	private static final String JAVAX_VALIDATION_END = ".message";
	private static final String JAVAX_VALIDATION_END_SUBSTITUTE = "";

	private final MessageHelper messageHelper;
	private final Model<?> model;
	private final List<Error> errors;
	private boolean focused = false;

	public ErrorBinderVisitor(Model<?> model, MessageHelper messageHelper, List<Error> errors) {
		this.messageHelper = messageHelper;
		this.errors = errors;
		this.model = model;
	}

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		B editor = context.getEditor();

		if (editor instanceof EditorError) {
			Class<?> propertyType = ModelUtils.resolveType(this.model, context.getPath());

			Path path = context.getPath();
			EditorError editorError = (EditorError) editor;
			editorError.clearErrors();

			List<Error> toDisplay = Lists.newArrayList();
			for (Error error : this.errors) {
				if (path.equals(error.getPath()) && !error.isConsumed()) {
					String messageKey = error.getMessageKey();
					messageKey = this.fixMessageKey(messageKey);
					String message = this.messageHelper.findMessage(propertyType, messageKey);
					if (error.getParameters() != null) {
						message = MessageHelper.replaceParams(message, error.getParameters());
					}
					if (message == null) {
						message = messageKey;
					}
					SimpleError errorToDisplay = new SimpleError(editorError, message, error.getValue(), error.getParameters());
					toDisplay.add(errorToDisplay);
					error.consume();
				}
			}
			if (!toDisplay.isEmpty() && editorError instanceof HasDrawable) {
				editorError.displayErrors(toDisplay);
				((HasDrawable) editorError).redraw();
				if (!focused && editorError instanceof Focusable) {
					focused = true;
					((Focusable) editorError).setFocus(true);
				}
			}
		}
		return true;
	}

	private String fixMessageKey(String messageKey) {
		if (messageKey == null) {
			return null;
		}
		String fixed = messageKey;
		if (fixed.startsWith("{") && fixed.endsWith("}")) {
			fixed = fixed.substring(1, fixed.length() - 1);
		}
		if (fixed.startsWith(ErrorBinderVisitor.JAVAX_VALIDATION_START)) {
			fixed =
				fixed.replaceFirst(ErrorBinderVisitor.JAVAX_VALIDATION_START,
					ErrorBinderVisitor.JAVAX_VALIDATION_START_SUBSTITUTE);
		}
		if (fixed.endsWith(ErrorBinderVisitor.JAVAX_VALIDATION_END)) {
			fixed =
				fixed.replace(ErrorBinderVisitor.JAVAX_VALIDATION_END, ErrorBinderVisitor.JAVAX_VALIDATION_END_SUBSTITUTE);
		}
		return fixed;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.FLUSH;
	}

}
