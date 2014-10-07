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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.user.client.ui.IsWidget;

import java.util.Collection;

import fr.putnami.pwt.core.editor.client.EditorCollection;
import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.editor.client.EditorValue;
import fr.putnami.pwt.core.editor.client.Visitor;
import fr.putnami.pwt.core.editor.client.factory.OutputFactory;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.base.EditorModel;
import fr.putnami.pwt.core.model.client.base.EditorProvider;
import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.model.client.base.HasEditorProvider;
import fr.putnami.pwt.core.model.client.base.HasOutputEditorFactory;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.model.ModelCollection;
import fr.putnami.pwt.core.model.client.visitor.ReadonlyVisitor;

public class OutputList<T> extends List
	implements EditorCollection<T>, EditorOutput<Collection<T>>, EditorModel<T>, HasDrawable,
	HasEditorProvider, HasOutputEditorFactory {

	private String path;
	private Model<T> model;
	private ModelDriver<Collection<T>> driver;
	private MessageHelper messageHelper;

	private OutputFactory outputFactory;
	private EditorProvider editorProvider;

	public OutputList() {
		this.setType(Type.INLINE);
	}

	public OutputList(OutputList<T> source) {
		this();
		if (source != null) {
			this.path = source.path;
			this.model = source.model;
			this.messageHelper = source.messageHelper;

			this.outputFactory = source.outputFactory;
		}
	}

	@Override
	public IsWidget cloneWidget() {
		return new OutputList<T>(this);
	}

	@Override
	public void initialize(Model<T> model, Visitor... visitors) {
		assert this.model == null : "model can not be set twice.";
		this.model = model;
		if (model instanceof ModelCollection) {
			this.driver = new ModelDriver<Collection<T>>((ModelCollection<T>) model);
		} else {
			this.driver = new ModelDriver<Collection<T>>(new ModelCollection<T>(List.class, model));
		}
		this.driver.setMessageHelper(this.messageHelper);
		this.driver.initialize(this, visitors);
		this.driver.accept(new ReadonlyVisitor(this, true, true));
	}

	@Override
	public <A extends EditorValue<T>> A getEditorForTraversal(int index) {
		EditorOutput output = this.editorProvider.getEditorForTraversal(true, index);
		if (output instanceof ListItem) {
			this.addListItem((ListItem) output);
		} else {
			ListItem listItem = new ListItem("");
			listItem.add(output);
			this.addListItem(listItem);
		}
		return (A) output;
	}

	@Override
	public void add(IsWidget child) {
		if (this.outputFactory == null && child instanceof OutputFactory) {
			this.outputFactory = (OutputFactory) child;
		} else {
			super.add(child); // List#add(IsWidget)
		}
	}

	@Override
	public String getPath() {
		return this.path;
	}

	@Override
	public void edit(Collection<T> value) {
		this.clear();
		this.driver.edit(value);
	}

	@Override
	public void setEditorProvider(EditorProvider provider) {
		this.editorProvider = provider;
	}

	@Override
	public void setMessageHelper(MessageHelper messageHelper) {
		this.messageHelper = messageHelper;
	}

	@Override
	public Model<T> getModel() {
		return this.model;
	}

	@Override
	public Collection<T> getValue() {
		return this.driver.getValue();
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public OutputFactory getOutputFactory() {
		return this.outputFactory;
	}

	@Override
	public void redraw() {
	}

}
