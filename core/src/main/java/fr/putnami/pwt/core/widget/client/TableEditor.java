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

import com.google.gwt.editor.client.LeafValueEditor;
import com.google.gwt.user.client.ui.IsWidget;

import java.util.Collection;
import java.util.Collections;

import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.EditorLeaf;
import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.Visitor;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.client.helper.TakesValueEditorWrapper;
import fr.putnami.pwt.core.editor.client.validator.Validator;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.base.EditorModel;
import fr.putnami.pwt.core.model.client.base.HasDriver;
import fr.putnami.pwt.core.model.client.exception.EditorModelNotInitializedException;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.model.ModelCollection;
import fr.putnami.pwt.core.model.client.visitor.ReadonlyVisitor;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumn;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class TableEditor<T> extends Table<T>
	implements HasDriver<Collection<T>, ModelDriver<Collection<T>>>, EditorLeaf, EditorOutput<Collection<T>>,
	EditorInput<Collection<T>>, EditorModel<T> {

	private MessageHelper messageHelper;
	private Model<T> model;
	private ModelDriver<Collection<T>> driver;

	private Pagination pagination;

	public TableEditor() {
		super();
	}

	protected TableEditor(TableEditor<T> source) {
		super(source);
		this.setPagination(WidgetUtils.cloneWidget(source.pagination));
		this.append(source.pagination);
	}

	@Override
	public IsWidget cloneWidget() {
		return new TableEditor<T>(this);
	}

	@Override
	protected TableBody<T> createBody(String bodyId) {
		TableBody<T> body = new TableEditorBody<T>(bodyId);
		body.setReadonly(this.getReadonly());
		return body;
	}

	@Override
	public Model<T> getModel() {
		return this.model;
	}

	@Override
	public void setReadonly(Boolean readonly) {
		super.setReadonly(readonly);
		if (this.driver != null) {
			this.driver.accept(new ReadonlyVisitor(this, readonly, true));
		}
	}

	@Override
	public void add(IsWidget w) {
		super.add(w);
		if (w instanceof Pagination) {
			this.setPagination((Pagination) w);
			this.append(w);
		}
		if (w instanceof AbstractTableColumn) {
			this.addColumn((AbstractTableColumn) w);
		}
	}

	@Override
	public void setMessageHelper(MessageHelper messageHelper) {
		this.messageHelper = messageHelper;
	}

	@Override
	public void initialize(Model<T> model, Visitor... visitors) {
		assert this.model == null : "model can not be set twice.";
		this.model = model;
		this.driver = new ModelDriver<Collection<T>>(new ModelCollection<T>(List.class, model));
		this.driver.setMessageHelper(this.messageHelper);
		this.driver.initialize(this, visitors);
		this.driver.accept(new ReadonlyVisitor(this, this.getReadonly(), true));
		if (this.pagination != null) {
			this.pagination.<T> getPaginationHelper().setDriver(driver);
			this.driver.registerVisitor(this.pagination.<T> getPaginationHelper());
		}
	}

	@Override
	public ModelDriver<Collection<T>> getDriver() {
		return this.driver;
	}

	@Override
	public void setDriver(ModelDriver<Collection<T>> driver) {
		// Nothing to do, initialised method must be call in order to collect contexts
	}

	@Override
	public Collection<T> flush() {
		getDriverOrThrow().flush();
		// FIXME TableOrder doesn't sort result, so we ask to sort result via TableEditor
		if (!this.driver.hasErrors()) {
			return ((TableEditorBody<T>) this.getDefaultBody()).flush();
		}
		return this.driver.getValue();
	}

	@Override
	public void edit(Collection<T> value) {
		getDriverOrThrow().edit(value);
	}

	@Override
	public LeafValueEditor<Collection<T>> asEditor() {
		return new TakesValueEditorWrapper<Collection<T>>(this);
	}

	public <A> boolean removeColumn(AbstractTableColumn<A> column) {
		this.ensureTableHead().removeColumn(column);
		TableBody<T> body = this.getDefaultBody();
		if (body instanceof TableEditorBody) {
			((TableEditorBody<T>) body).removeColumn(column);
		}
		return true;
	}

	public void setPagination(Pagination pagination) {
		this.pagination = pagination;
		if (this.driver != null) {
			this.pagination.<T> getPaginationHelper().setDriver(driver);
			this.driver.registerVisitor(this.pagination.<T> getPaginationHelper());
		}
	}

	private <A> void addColumn(AbstractTableColumn<A> column) {
		this.ensureTableHead().add(column);
		this.getDefaultBody().add(column);
	}

	@Override
	public Collection<T> getValue() {
		return getDriverOrThrow().getValue();
	}

	@Override
	public boolean hasErrors() {
		return this.driver == null ? false : this.driver.hasErrors();
	}

	@Override
	public Iterable<Error> getErrors() {
		return this.driver == null ? Collections.<Error> emptyList() : this.driver.getErrors();
	}

	@Override
	public void addValidator(Validator<Collection<T>> validator) {
		// TODO check if the validator must not be added to the driver.
	}

	private ModelDriver<Collection<T>> getDriverOrThrow() {
		if (this.driver == null) {
			throw new EditorModelNotInitializedException(this);
		}
		return this.driver;
	}

}
