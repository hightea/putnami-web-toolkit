/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.model.client.visitor;

import com.google.common.collect.Iterables;
import com.google.common.collect.LinkedListMultimap;
import com.google.common.collect.Multimap;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorError;
import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.impl.SimpleError;
import fr.putnami.pwt.core.editor.client.util.PathUtils;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.base.HasDriver;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.util.ModelUtils;

public class FlusherVisitor<T> extends AbstractVisitor {

	private final Multimap<Context<?>, Error> errors = LinkedListMultimap.create();

	private final Model<T> model;
	private T targetValue;

	public FlusherVisitor(Model<T> model, T targetValue) {
		this.model = model;
		this.targetValue = targetValue;
	}

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		B editor = context.getEditor();

		if (editor instanceof EditorError) {
			EditorError editorError = (EditorError) editor;
			editorError.clearErrors();
		}
		A value = null;
		Path path = context.getPath();
		if (editor instanceof EditorInput) {
			if (!PathUtils.isRoot(path)) {
				value = ((EditorInput<A>) editor).flush();
			}
			if (editor instanceof HasDriver && !context.isRootContext()) {
				HasDriver hasDriverEditor = (HasDriver) editor;
				hasDriverEditor.getDriver().accept(this);
			}

			EditorInput<A> inputEditor = (EditorInput<A>) editor;
			if (inputEditor.hasErrors()) {
				for (Error error : inputEditor.getErrors()) {
					if (error instanceof SimpleError) {
						((SimpleError) error).setPath(path);
					}
					errors.put(context, error);
				}
			}

			targetValue = ModelUtils.bindValue(targetValue, this.model, path, value);
		}

		return true;
	}

	public T getTargetValue() {
		return this.targetValue;
	}

	public boolean hasErrors() {
		return this.errors != null && !this.errors.isEmpty();
	}

	public Iterable<Error> getErrors() {
		return Iterables.unmodifiableIterable(this.errors.values());
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.FLUSH;
	}
}
